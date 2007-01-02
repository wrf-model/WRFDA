SUBROUTINE da_get_innov_vector_geoamv( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!
!  History:
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: Calculates innovation vector does QC for  Geo. AMV's
!               
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it      ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb      ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp      ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)    :: ob      ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv      ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL, DIMENSION(1:max_ob_levels) :: model_u  ! Model value u at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_v  ! Model value v at ob location.

   REAL, DIMENSION(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.
!
   INTEGER           :: itu,ituf,itvv,itvvf
!
   IF ( iv % num_geoamv > 0 ) THEN

   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

      DO n=iv%ob_numb(iv%current_ob_time-1)%geoamv + 1, iv%ob_numb(iv%current_ob_time)%geoamv

       if(iv%geoamv(n)%loc%proc_domain_with_halo) then

!        [1.3] Get horizontal interpolation weights:

        num_levs = iv % geoamv(n) % info % levels
         if ( num_levs < 1 ) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0

         i = iv%geoamv(n)%loc%i
         j = iv%geoamv(n)%loc%j
         dx = iv%geoamv(n)%loc%dx
         dy = iv%geoamv(n)%loc%dy
         dxm = iv%geoamv(n)%loc%dxm
         dym = iv%geoamv(n)%loc%dym

         do k=xp%kts,xp%kte
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo





        do k=1, iv % geoamv(n) % info % levels
            iv%geoamv(n)%zk(k)=missing_r

            IF ( iv % geoamv(n) % p(k) > 1.0 ) THEN

               call to_zk(iv % geoamv(n) % p(k), v_p, xp, v_interp_p, iv%geoamv(n)%zk(k))
            ENDIF

            if(iv%geoamv(n)%zk(k) < 0.0) then
               iv % geoamv(n) % u(k) % qc = missing
               iv % geoamv(n) % v(k) % qc = missing
            endif
         enddo

#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                             model_u, max_ob_levels, iv%geoamv(n)%zk, num_levs )
         call Interp_lin_3D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                             model_v, max_ob_levels, iv%geoamv(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_u(1), max_ob_levels, iv%geoamv(n)%zk(1), num_levs )
         call Interp_lin_3D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_v(1), max_ob_levels, iv%geoamv(n)%zk(1), num_levs )
#endif


        do k = 1, iv % geoamv(n) % info % levels


            iv % geoamv(n) % u(k) % inv = 0.0
            iv % geoamv(n) % v(k) % inv = 0.0


            IF ( ob % geoamv(n) % u(k) > missing_r .AND. &
                 iv % geoamv(n) % u(k) % qc >= obs_qc_pointer ) THEN

                 iv % geoamv(n) % u(k) % inv = ob % geoamv(n) % u(k) - &
                                              model_u(k)
            ENDIF

            IF ( ob % geoamv(n) % v(k) > missing_r .AND. &
                 iv % geoamv(n) % v(k) % qc >= obs_qc_pointer ) THEN

                 iv % geoamv(n) % v(k) % inv = ob % geoamv(n) % v(k) - &
                                              model_v(k)
            ENDIF

         END DO

!------------------------------------------------------------------------
!  Perform optional maximum error check:
!------------------------------------------------------------------------

        IF ( check_max_iv ) CALL da_check_max_iv_geoamv(it, iv % geoamv(n), &
                                itu,ituf,itvv,itvvf)

       endif

      END DO
       write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
       ', Total Rejections for Geo. AMVs follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
      'Finally Total Geo. AMVs rejections ',ituf+itvvf,' on ',itu+itvv
   ENDIF

END SUBROUTINE da_get_innov_vector_geoamv

