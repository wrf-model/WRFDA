SUBROUTINE da_get_innov_vector_satob ( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!
!  History:
!
!
!     Additions:        03/19/2003                    S. R. H. Rizvi
!
!               1. DA_Check_Max_IV is called for each outer iteration
!
!               2. Extra diagnostics added for generating detailed information
!                  about rejected obs in various outer iterations
!
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it      ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb      ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp      ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)    :: ob      ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv      ! O-B structure.

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: model_u  ! Model value u at oblocation.
   REAL                         :: model_v  ! Model value v at oblocation.


   REAL, DIMENSION(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.
!
   INTEGER           :: itu,ituf,itvv,itvvf
!
   IF ( iv % num_satob > 0 ) THEN

   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

      DO n=iv%ob_numb(iv%current_ob_time-1)%satob + 1, iv%ob_numb(iv%current_ob_time)%satob

       if(iv%satob(n)%loc%proc_domain_with_halo) then

!        [1.3] Get horizontal interpolation weights:

         i = iv%satob(n)%loc%i
         j = iv%satob(n)%loc%j
         dx = iv%satob(n)%loc%dx
         dy = iv%satob(n)%loc%dy
         dxm = iv%satob(n)%loc%dxm
         dym = iv%satob(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo

         IF ( iv % satob(n) % p > 1.0 ) THEN
            call to_zk(iv % satob(n) % p, v_p, xp, v_interp_p, iv%satob(n)%zk)
         ELSE IF ( iv % satob(n) % h > 0.0 ) THEN
            call to_zk(iv % satob(n) % h, v_h, xp, v_interp_h, iv%satob(n)%zk)
         ENDIF

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         iv % satob(n) % u % inv = 0.0
         iv % satob(n) % v % inv = 0.0

         if(iv%satob(n)%zk < 0.0) then
            iv % satob(n) % u % qc = missing
            iv % satob(n) % v % qc = missing
         else

!        [1.4] Interpolate horizontally from dot points:

#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                 model_u, iv%satob(n)%zk )
         call Interp_Obs_lin_2D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                 model_v, iv%satob(n)%zk )
#else
         call Interp_Obs_lin_2D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_u, iv%satob(n)%zk )
         call Interp_Obs_lin_2D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_v, iv%satob(n)%zk )
#endif
!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

!------------------------------------------------------------------------
!        [4.0] Fast interpolation at dot points:
!------------------------------------------------------------------------

            IF ( ob % satob(n) % u > missing_r .AND. &
                 iv % satob(n) % u % qc >= obs_qc_pointer ) THEN

                 iv % satob(n) % u % inv = ob % satob(n) % u - model_u
            ENDIF

            IF ( ob % satob(n) % v > missing_r .AND. &
                 iv % satob(n) % v % qc >= obs_qc_pointer ) THEN

                 iv % satob(n) % v % inv = ob % satob(n) % v - model_v
            ENDIF

         endif

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

          IF ( check_max_iv ) CALL da_check_max_iv_satob(it, iv % satob(n), &
                        itu,ituf,itvv,itvvf )

       endif

      END DO
!
   write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Satob follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
      'Finally all Satob rejections ',ituf+itvvf,' on ',itu +itvv

   ENDIF

END SUBROUTINE da_get_innov_vector_satob

