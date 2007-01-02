SUBROUTINE da_get_innov_vector_profiler ( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!
!  History:
!
!       Created:        01/13/2003                    S. R. H. Rizvi
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

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   REAL, DIMENSION(1:max_ob_levels) :: model_u  ! Model value u at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_v  ! Model value v at ob location.

   REAL, DIMENSION(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.
   INTEGER           :: itu,ituf,itvv,itvvf

   IF ( iv % num_profiler > 0 ) THEN
   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

      DO n=iv%ob_numb(iv%current_ob_time-1)%profiler + 1, iv%ob_numb(iv%current_ob_time)%profiler

       if(iv%profiler(n)%loc%proc_domain_with_halo) then

         num_levs = iv % profiler(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0
      
!        [1.3] Get horizontal interpolation weights:

         i = iv%profiler(n)%loc%i
         j = iv%profiler(n)%loc%j
         dx = iv%profiler(n)%loc%dx
         dy = iv%profiler(n)%loc%dy
         dxm = iv%profiler(n)%loc%dxm
         dym = iv%profiler(n)%loc%dym

         do k=xp%kts,xp%kte
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo

         do k=1, iv % profiler(n) % info % levels

            iv%profiler(n)%zk(k)=missing_r

            IF ( iv % profiler(n) % p(k) > 1.0 ) THEN
               call to_zk(iv % profiler(n) % p(k), v_p, xp, v_interp_p, iv%profiler(n)%zk(k))
            ENDIF

            if(iv%profiler(n)%zk(k) < 0.0) then
               iv % profiler(n) % u(k) % qc = missing
               iv % profiler(n) % v(k) % qc = missing
            endif
         enddo

!        [1.4] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                             model_u, max_ob_levels, iv%profiler(n)%zk, num_levs )
         call Interp_lin_3D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                             model_v, max_ob_levels, iv%profiler(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_u(1), max_ob_levels, iv%profiler(n)%zk(1), num_levs )
         call Interp_lin_3D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_v(1), max_ob_levels, iv%profiler(n)%zk(1), num_levs )
#endif
!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv % profiler(n) % info % levels

            iv % profiler(n) % u(k) % inv = 0.0
            iv % profiler(n) % v(k) % inv = 0.0

!------------------------------------------------------------------------
!           [4.0] Fast interpolation:
!------------------------------------------------------------------------

            IF ( ob % profiler(n) % u(k) > missing_r .AND. &
                 iv % profiler(n) % u(k) % qc >= obs_qc_pointer ) THEN

                 iv % profiler(n) % u(k) % inv = ob % profiler(n) % u(k) - &
                                              model_u(k)
            ENDIF

            IF ( ob % profiler(n) % v(k) > missing_r .AND. &
                 iv % profiler(n) % v(k) % qc >= obs_qc_pointer ) THEN

                 iv % profiler(n) % v(k) % inv = ob % profiler(n) % v(k) - &
                                              model_v(k)
            ENDIF

         END DO

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

         IF ( check_max_iv )   &  
         CALL da_check_max_iv_profiler(it, iv % profiler(n), &
                 itu,ituf,itvv,itvvf)
            
       endif

      END DO

       write(unit = check_max_iv_unit, FMT ='(A,i5,A)')&
       'For outer iteration ',it, ', Total Rejections for Profiler follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,  &
      'Finally Total Profiler rejections ',ituf+itvvf,' on ',itu +itvv

   ENDIF

END SUBROUTINE da_get_innov_vector_profiler

