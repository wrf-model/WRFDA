subroutine da_transform_xtoy_ships (xa, xb, iv, xp, y)

   implicit none

   type (x_type),     intent(inout)  :: xa       ! gridded analysis increment.
   type (xb_type),    intent(in)     :: xb       ! first guess state.
   type (ob_type),    intent(in)     :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in)     :: xp       ! Domain decomposition vars.
   type (y_type),     intent(inout)  :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_ships > 0) then
      !xyh y%ships(:)%u = 0.0
      !xyh y%ships(:)%v = 0.0
      !xyh y%ships(:)%t = 0.0
      !xyh y%ships(:)%p = 0.0
      !xyh y%ships(:)%q = 0.0

      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
            ! [1.1] Get horizontal interpolation weights:

            i = iv%ships(n)%loc%i
            dy = iv%ships(n)%loc%dy
            dym = iv%ships(n)%loc%dym
            j = iv%ships(n)%loc%j
            dx = iv%ships(n)%loc%dx
            dxm = iv%ships(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%q, iv%ships(n)%zk)

            call da_interp_lin_2d(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                               i, j, dx, dy, dxm, dym, &
                               y%ships(n)%p)
#else
            call da_interp_obs_lin_2d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, &
                                   dx, dy, dxm, dym, &
                                   y%ships(n)%u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, &
                                   dx, dy, dxm, dym, &
                                   y%ships(n)%v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, &
                                   dx, dy, dxm, dym, &
                                   y%ships(n)%t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, &
                                   dx, dy, dxm, dym, &
                                   y%ships(n)%q, iv%ships(n)%zk)

            call da_interp_lin_2d(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                               i, j, dx, dy, dxm, dym, y%ships(n)%p)
#endif
         end do
      else if (sfc_assi_options == 2) then
          ! [2.0] Surface assimilation approach 2
          do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
             call da_transform_xtopsfc(xb,xa,xp,iv%ships(n),y%ships(n))
         end do
      end if
   end if

end subroutine da_transform_xtoy_ships


