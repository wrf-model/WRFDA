subroutine da_transform_xtoy_airep_adj(iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   do n=iv%ob_numb(iv%current_ob_time-1)%airep + 1, iv%ob_numb(iv%current_ob_time)%airep
      k = iv % airep(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d_adj(jo_grad_x%u, iv%airep(n)%loc, jo_grad_y%airep(n)%u, k, iv%airep(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%v, iv%airep(n)%loc, jo_grad_y%airep(n)%v, k, iv%airep(n)%zk, k)
      call da_interp_lin_3d_adj(jo_grad_x%t, iv%airep(n)%loc, jo_grad_y%airep(n)%t, k, iv%airep(n)%zk, k)
   end do

end subroutine da_transform_xtoy_airep_adj


