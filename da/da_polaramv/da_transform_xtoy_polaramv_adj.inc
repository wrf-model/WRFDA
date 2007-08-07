subroutine da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Polar AMVs 
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n, k     ! Loop counter.

   do n=iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, &
      iv%ob_numb(iv%current_ob_time)%polaramv
      k = iv % polaramv(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d_adj(jo_grad_x % u, iv%polaramv(n)%loc, &
         jo_grad_y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x % v, iv%polaramv(n)%loc, &
         jo_grad_y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
   end do

end subroutine da_transform_xtoy_polaramv_adj


