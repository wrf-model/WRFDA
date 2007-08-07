subroutine da_transform_xtoy_pseudo_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   if (trace_use) call da_trace_entry("da_transform_xtoy_pseudo_adj")

   if (iv%num_pseudo > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%pseudo + 1, iv%ob_numb(iv%current_ob_time)%pseudo
         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%pseudo(n)%loc, &
            jo_grad_y%pseudo(n)%u, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%pseudo(n)%loc, &
            jo_grad_y%pseudo(n)%v, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%q, iv%pseudo(n)%loc, &
            jo_grad_y%pseudo(n)%q, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%p, iv%pseudo(n)%loc, &
            jo_grad_y%pseudo(n)%p, iv%pseudo(n)%zk)
         call da_interp_obs_lin_2d_adj(jo_grad_x%t, iv%pseudo(n)%loc, &
            jo_grad_y%pseudo(n)%t, iv%pseudo(n)%zk)
      end do
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_pseudo_adj")

end subroutine da_transform_xtoy_pseudo_adj


