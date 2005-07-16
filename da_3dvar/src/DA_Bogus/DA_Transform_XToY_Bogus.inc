SUBROUTINE da_transform_xtoy_bogus (xa, xb, iv, xp, y )

!---------------------------------------------------------------------
!  This subroutine is the linearized bogus observation operators.
!  It includes slp, winds.          Ying Zhang, September 2003.
!  Port the algorithm to WRF frame. Xianyan Zhang and Qingnong Xiao, February 2005.
!---------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (x_type),  INTENT(IN)     :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)     :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT)   :: y        ! y = h (xa) (linear)

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! 
   REAL                           :: dy, dym  !
   REAL, DIMENSION(xp%kts:xp%kte) :: model_t,model_t9
   REAL, DIMENSION(xp%kts:xp%kte) :: model_q,model_q9
   REAL, DIMENSION(xp%kts:xp%kte) :: model_p_c,model_p_c9
   REAL                           :: psfc_loc,terr_loc,psfc_loc9
   
   IF ( iv%num_bogus > 0 ) THEN

      do n=1, iv % num_bogus

       if(iv%bogus(n)%loc%proc_domain_with_halo) then

         y%bogus(n)%slp  = 0.0
         y%bogus(n)%u(:) = 0.0
         y%bogus(n)%v(:) = 0.0
         y%bogus(n)%t(:) = 0.0
         y%bogus(n)%q(:) = 0.0

         num_levs = iv % bogus(n) % info % levels

         if ( num_levs < 1 ) cycle

!        [1.1] Get cross pt. horizontal interpolation weights:

         i = iv%bogus(n)%loc%i
         dy = iv%bogus(n)%loc%dy
         dym = iv%bogus(n)%loc%dym
         j = iv%bogus(n)%loc%j
         dx = iv%bogus(n)%loc%dx
         dxm = iv%bogus(n)%loc%dxm

!        [1.2] Interpolate horizontally from cross points:

         do k = xp%kts, xp%kte
       
            model_t9(k) = dym*(dxm*xa%t(i,j,k) + dx*xa%t(i+1,j,k)) &
                          + dy *(dxm*xa%t(i,j+1,k) + dx*xa%t(i+1,j+1,k))
            model_t(k) = dym*(dxm*xb%t(i,j,k) + dx*xb%t(i+1,j,k)) &
                          + dy *(dxm*xb%t(i,j+1,k) + dx*xb%t(i+1,j+1,k))
            model_q9(k) = dym*(dxm*xa%q(i,j,k) + dx*xa%q(i+1,j,k)) &
                          + dy *(dxm*xa%q(i,j+1,k) + dx*xa%q(i+1,j+1,k))
            model_q(k) = dym*(dxm*xb%q(i,j,k) + dx*xb%q(i+1,j,k)) &
                          + dy *(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
            model_p_c9(k) = dym*(dxm*xa%p(i,j,k) + dx*xa%p(i+1,j,k)) &
                          + dy *(dxm*xa%p(i,j+1,k) + dx*xa%p(i+1,j+1,k))
            model_p_c(k) = dym*(dxm*xb%p(i,j,k) + dx*xb%p(i+1,j,k)) &
                          + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))

         enddo

         terr_loc = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) &
                    + dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))
         psfc_loc = dym*(dxm*xb%psfc(i,j)   + dx*xb%psfc(i+1,j)) &
                    + dy *(dxm*xb%psfc(i,j+1) + dx*xb%psfc(i+1,j+1))
         psfc_loc9 = dym*(dxm*xa%psfc(i,j)   + dx*xa%psfc(i+1,j)) &
                    + dy *(dxm*xa%psfc(i,j+1) + dx*xa%psfc(i+1,j+1))

         call DA_TPQ_To_SLP_lin (model_t, model_q, model_p_c,    &
                                terr_loc, psfc_loc,   &
                                model_t9, model_q9,  &
                                model_p_c9, psfc_loc9, y%bogus(n)%slp, xp) 

!        [1.4] Interpolate horizontally from dot points:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%u, num_levs, iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%v, num_levs, iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%t, num_levs, iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%q, num_levs, iv%bogus(n)%zk, num_levs)
#else
         call Interp_lin_3D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%u(1), num_levs, iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%v(1), num_levs, iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%t(1), num_levs, iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%bogus(n)%q(1), num_levs, iv%bogus(n)%zk(1), num_levs)
#endif

       end if

      end do

   ENDIF

END SUBROUTINE da_transform_xtoy_bogus

