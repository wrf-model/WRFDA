subroutine da_transform_xtoy_synop_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_synop_adj")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,synop) + 1, iv%plocal(iv%time,synop)
         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%synop(n)%loc, jo_grad_y%synop(n)%u, iv%synop(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%synop(n)%loc, jo_grad_y%synop(n)%v, iv%synop(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%t, iv%synop(n)%loc, jo_grad_y%synop(n)%t, iv%synop(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%q, iv%synop(n)%loc, jo_grad_y%synop(n)%q, iv%synop(n)%zk)

         call da_interp_lin_2d_adj(jo_grad_x % psfc, iv%synop(n)%loc, jo_grad_y%synop(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%plocal(iv%time-1,synop) + 1, iv%plocal(iv%time,synop)
         call da_transform_xtopsfc_adj(grid,iv%synop(n), jo_grad_y%synop(n),jo_grad_x)
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_synop_adj")

end subroutine da_transform_xtoy_synop_adj


