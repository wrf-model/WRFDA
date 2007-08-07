subroutine da_transform_xtoy_profiler_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   do n=iv%ob_numb(iv%current_ob_time-1)%profiler + 1, iv%ob_numb(iv%current_ob_time)%profiler
      k = iv % profiler(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d_adj(jo_grad_x % u, iv%profiler(n)%loc, &
         jo_grad_y%profiler(n)%u, k, iv%profiler(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x % v, iv%profiler(n)%loc, &
         jo_grad_y%profiler(n)%v, k, iv%profiler(n)%zk, k)
   end do 

end subroutine da_transform_xtoy_profiler_adj


