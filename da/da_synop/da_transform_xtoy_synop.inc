subroutine da_transform_xtoy_synop (xa, xb, iv, xp, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),  intent(inoUT) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in) :: xp       ! Domain decomposition vars.
   type (y_type), intent(inout)  :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_synop > 0) then

      !xyh y%synop(:)%u = 0.0
      !xyh y%synop(:)%v = 0.0
      !xyh y%synop(:)%t = 0.0
      !xyh y%synop(:)%p = 0.0
      !xyh y%synop(:)%q = 0.0

      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop

            ! [1.1] Get horizontal interpolation weights:

            i = iv%synop(n)%loc%i
            j = iv%synop(n)%loc%j
            dx = iv%synop(n)%loc%dx
            dy = iv%synop(n)%loc%dy
            dxm = iv%synop(n)%loc%dxm
            dym = iv%synop(n)%loc%dym

            ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%u, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%v, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%t, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%q, iv%synop(n)%zk)

            call da_interp_lin_2d(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, y%synop(n)%p)
#else
            call da_interp_obs_lin_2d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%u, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%v, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%t, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    y%synop(n)%q, iv%synop(n)%zk)

            call da_interp_lin_2d(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, y%synop(n)%p)
#endif

         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop
            call da_transform_xtopsfc(xb, xa, xp, iv%synop(n), y%synop(n))
         end do
      end if
   end if

end subroutine da_transform_xtoy_synop


