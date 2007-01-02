SUBROUTINE da_get_innov_vector_airep ( it, xb, xp, ob, iv )
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
   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.


   REAL                            :: zk       ! Interpolation vertical coordinator.
   REAL, DIMENSION(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.


!
   INTEGER           :: itu,ituf,itvv,itvvf,itt,ittf
!

   IF ( iv % num_airep > 0 ) THEN

   itu   = 0; itvv    = 0; itt  = 0;
   ituf  = 0; itvvf   = 0; ittf = 0;

      DO n=iv%ob_numb(iv%current_ob_time-1)%airep + 1, iv%ob_numb(iv%current_ob_time)%airep

       if(iv%airep(n)%loc%proc_domain_with_halo) then

         num_levs = iv % airep(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0
         model_t(:) = 0.0

!        [1.1] Get horizontal interpolation weights:

         i = iv%airep(n)%loc%i
         j = iv%airep(n)%loc%j
         dx = iv%airep(n)%loc%dx
         dy = iv%airep(n)%loc%dy
         dxm = iv%airep(n)%loc%dxm
         dym = iv%airep(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo

         num_levs=0
         do k=1, iv % airep(n) % info % levels

            iv%airep(n)%zk(k)=missing_r

            IF ( iv % airep(n) % p(k) > 1.0 ) THEN
               call to_zk(iv % airep(n) % p(k), v_p, xp, v_interp_p, iv%airep(n)%zk(k))
            ELSE IF ( iv % airep(n) % h(k) > 0.0 ) THEN
               call to_zk(iv % airep(n) % h(k), v_h, xp, v_interp_h, iv%airep(n)%zk(k))
            ENDIF

            if(iv%airep(n)%zk(k) < 0.0) then
               iv % airep(n) % u(k) % qc = missing
               iv % airep(n) % v(k) % qc = missing
               iv % airep(n) % t(k) % qc = missing
            endif
         enddo

         num_levs = iv % airep(n) % info % levels

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                             model_u, max_ob_levels, iv%airep(n)%zk, num_levs)
                    
         call Interp_lin_3D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                             model_v, max_ob_levels, iv%airep(n)%zk, num_levs)
                    
         call Interp_lin_3D( xb % t, xp, i, j, dx, dy, dxm, dym, &
                             model_t, max_ob_levels, iv%airep(n)%zk, num_levs)
                    
#else
         call Interp_lin_3D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_u(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)
                    
         call Interp_lin_3D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_v(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)
                    
         call Interp_lin_3D( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_t(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)
#endif

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv % airep(n) % info % levels

            iv % airep(n) % u(k) % inv = 0.0
            iv % airep(n) % v(k) % inv = 0.0
            iv % airep(n) % t(k) % inv = 0.0

!------------------------------------------------------------------------
!              [3.0] Fast interpolation:
!------------------------------------------------------------------------

               IF ( ob % airep(n) % u(k) > missing_r .AND. &
                    iv % airep(n) % u(k) % qc >= obs_qc_pointer ) THEN

                    iv % airep(n) % u(k) % inv = ob % airep(n) % u(k) - &
                                                 model_u(k)
               ENDIF

               IF ( ob % airep(n) % v(k) > missing_r .AND. &
                    iv % airep(n) % v(k) % qc >= obs_qc_pointer ) THEN

                    iv % airep(n) % v(k) % inv = ob % airep(n) % v(k) - &
                                                 model_v(k)
               ENDIF

               IF ( ob % airep(n) % t(k) > missing_r .AND. &
                    iv % airep(n) % t(k) % qc >= obs_qc_pointer ) THEN

                    iv % airep(n) % t(k) % inv = ob % airep(n) % t(k) - &
                                                 model_t(k)
               ENDIF

         END DO

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------
         IF ( check_max_iv ) CALL da_check_max_iv_airep(it, iv % airep(n), &
                        itu,ituf,itvv,itvvf,itt,ittf)
       endif

      END DO
!
      if(print_detail > 10) then
      write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Airep follows:'
      WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
      'Number of failed temperature observations:',ittf, ' on ',itt,   &
      'Finally Total Airep rejections ',ituf+itvvf+ittf,' on ',itu +itvv +itt
      endif
   ENDIF

END SUBROUTINE da_get_innov_vector_airep

