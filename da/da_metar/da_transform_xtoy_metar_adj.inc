subroutine da_transform_xtoy_metar_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid          ! first guess state.
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_metar_adj")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,metar) + 1, iv%plocal(iv%time,metar)

         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%metar(n)%loc, jo_grad_y%metar(n)%u, iv%metar(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%metar(n)%loc, jo_grad_y%metar(n)%v, iv%metar(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%t, iv%metar(n)%loc, jo_grad_y%metar(n)%t, iv%metar(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%q, iv%metar(n)%loc, jo_grad_y%metar(n)%q, iv%metar(n)%zk)

         call da_interp_lin_2d_adj(jo_grad_x % psfc, iv%metar(n)%loc, jo_grad_y%metar(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%plocal(iv%time-1,metar) + 1, iv%plocal(iv%time,metar)
         call da_transform_xtopsfc_adj(grid,iv%metar(n), jo_grad_y%metar(n),jo_grad_x)
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_metar_adj")

end subroutine da_transform_xtoy_metar_adj


