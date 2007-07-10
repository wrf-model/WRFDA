subroutine da_transform_xtoy_ships_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
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
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, i, j, dx, dy, & 
               dxm, dym, jo_grad_y%ships(n)%t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, i, j, dx, dy, &
               dxm, dym, jo_grad_y%ships(n)%q, iv%ships(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, ims, ime, &
               jms, jme, i, j, dx, dy, dxm, dym, jo_grad_y%ships(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
            iv%ob_numb(iv%current_ob_time)%ships
            call da_transform_xtopsfc_adj(grid,iv%ships(n), &
               jo_grad_y%ships(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_ships_adj


