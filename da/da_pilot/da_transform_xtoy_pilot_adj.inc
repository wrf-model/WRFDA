subroutine da_transform_xtoy_pilot_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot
      k = iv % pilot(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d_adj(jo_grad_x%u, iv%pilot(n)%loc, jo_grad_y%pilot(n)%u, k, iv%pilot(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%v, iv%pilot(n)%loc, jo_grad_y%pilot(n)%v, k, iv%pilot(n)%zk, k)
   end do

end subroutine da_transform_xtoy_pilot_adj


