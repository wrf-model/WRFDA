subroutine da_transform_xtoy_airsr_adj(iv, jo_grad_y, jo_grad_x)

   !----------------------------------------------------------------------
   ! Purpose: Does adjoint computation at AIRS retrieval locations
   !----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm
   real    :: dy, dym

   do n=iv%ob_numb(iv%current_ob_time-1)%airsr + 1, iv%ob_numb(iv%current_ob_time)%airsr
      k = iv % airsr(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d_adj(jo_grad_x%t, iv%airsr(n)%loc, jo_grad_y%airsr(n)%t, k, iv%airsr(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%q, iv%airsr(n)%loc, jo_grad_y%airsr(n)%q, k, iv%airsr(n)%zk, k)
   end do

end subroutine da_transform_xtoy_airsr_adj


