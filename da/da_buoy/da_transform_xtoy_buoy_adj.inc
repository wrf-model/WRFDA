subroutine da_transform_xtoy_buoy_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_buoy_adj")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,buoy) + 1, iv%plocal(iv%time,buoy)

         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%buoy(n)%loc, jo_grad_y%buoy(n)%u, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%buoy(n)%loc, jo_grad_y%buoy(n)%v, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%t, iv%buoy(n)%loc, jo_grad_y%buoy(n)%t, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%q, iv%buoy(n)%loc, jo_grad_y%buoy(n)%q, iv%buoy(n)%zk)

         call da_interp_lin_2d_adj(jo_grad_x % psfc, iv%buoy(n)%loc, jo_grad_y%buoy(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%plocal(iv%time-1,buoy) + 1, iv%plocal(iv%time,buoy)
         call da_transform_xtopsfc_adj(grid,iv%buoy(n), jo_grad_y%buoy(n), jo_grad_x)
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_buoy_adj")

end subroutine da_transform_xtoy_buoy_adj


