subroutine da_transform_xtoy_buoy_adj(xb, iv, xp, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_buoy > 0) then
      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy

            ! [1.1] Get horizontal interpolation weights:

            i = iv%buoy(n)%loc%i
            dy = iv%buoy(n)%loc%dy
            dym = iv%buoy(n)%loc%dym
            j = iv%buoy(n)%loc%j
            dx = iv%buoy(n)%loc%dx
            dxm = iv%buoy(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                     jo_grad_y%buoy(n)%u, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                     jo_grad_y%buoy(n)%v, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                     jo_grad_y%buoy(n)%t, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                     jo_grad_y%buoy(n)%q, iv%buoy(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                    i, j, dx, dy, dxm, dym, jo_grad_y%buoy(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy
            call da_transform_xtopsfc_adj(xb,xp,iv%buoy(n), jo_grad_y%buoy(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_buoy_adj


