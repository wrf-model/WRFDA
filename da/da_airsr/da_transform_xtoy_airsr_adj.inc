subroutine da_transform_xtoy_airsr_adj(iv, jo_grad_y, jo_grad_x)

   !----------------------------------------------------------------------
   ! Purpose: Does adjoint computation at AIRS retrieval locations
   !----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airsr_adj")

   do n=iv%plocal(iv%time-1,airsr) + 1, iv%plocal(iv%time,airsr)
      k = iv % airsr(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d_adj(jo_grad_x%t, iv%airsr(n)%loc, jo_grad_y%airsr(n)%t, k, iv%airsr(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%q, iv%airsr(n)%loc, jo_grad_y%airsr(n)%q, k, iv%airsr(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airsr_adj")

end subroutine da_transform_xtoy_airsr_adj


