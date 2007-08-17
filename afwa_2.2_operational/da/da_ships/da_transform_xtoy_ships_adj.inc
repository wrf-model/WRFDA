subroutine da_transform_xtoy_ships_adj(xb, iv, xp, jo_grad_y, jo_grad_x)

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
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !


   if (iv%num_ships > 0) then
      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
            iv%ob_numb(iv%current_ob_time)%ships

            ! [1.1] Get horizontal interpolation weights:

            i = iv%ships(n)%loc%i
            dy = iv%ships(n)%loc%dy
            dym = iv%ships(n)%loc%dym
            j = iv%ships(n)%loc%j
            dx = iv%ships(n)%loc%dx
            dxm = iv%ships(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, xp, i, j, dx, dy, & 
               dxm, dym, jo_grad_y%ships(n)%t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%q, iv%ships(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, xp%ims, xp%ime, &
               xp%jms, xp%jme, i, j, dx, dy, dxm, dym, jo_grad_y%ships(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
            iv%ob_numb(iv%current_ob_time)%ships
            call da_transform_xtopsfc_adj(xb,xp,iv%ships(n), &
                                               jo_grad_y%ships(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_ships_adj


