subroutine da_transform_xtoy_synop_adj(xb, iv, xp, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j, kz ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_synop > 0) then
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
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                            jo_grad_y%synop(n)%u, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                            jo_grad_y%synop(n)%v, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                            jo_grad_y%synop(n)%t, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                            jo_grad_y%synop(n)%q, iv%synop(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                   i, j, dx, dy, dxm, dym, jo_grad_y%synop(n)%p)
#else
            call da_interp_obs_lin_2d_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                       jo_grad_y%synop(n)%u, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                       jo_grad_y%synop(n)%v, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                       jo_grad_y%synop(n)%t, iv%synop(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                       jo_grad_y%synop(n)%q, iv%synop(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc(xp%ims,xp%jms), &
                                   xp%ims, xp%ime, xp%jms, xp%jme, &
                                   i, j, dx, dy, dxm, dym, jo_grad_y%synop(n)%p)
#endif
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop
            call da_transform_xtopsfc_adj(xb,xp,iv%synop(n), &
                                            jo_grad_y%synop(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_synop_adj


