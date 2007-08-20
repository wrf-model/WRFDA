subroutine da_transform_xtoy_ships_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ships_adj")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,ships) + 1, iv%plocal(iv%time,ships)
         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%ships(n)%loc, jo_grad_y%ships(n)%u, iv%ships(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%ships(n)%loc, jo_grad_y%ships(n)%v, iv%ships(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%t, iv%ships(n)%loc, jo_grad_y%ships(n)%t, iv%ships(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%q, iv%ships(n)%loc, jo_grad_y%ships(n)%q, iv%ships(n)%zk)

         call da_interp_lin_2d_adj(jo_grad_x % psfc, iv%ships(n)%loc, jo_grad_y%ships(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%plocal(iv%time-1,ships) + 1, &
         iv%plocal(iv%time,ships)
         call da_transform_xtopsfc_adj(grid,iv%ships(n), jo_grad_y%ships(n),jo_grad_x)
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ships_adj")

end subroutine da_transform_xtoy_ships_adj


