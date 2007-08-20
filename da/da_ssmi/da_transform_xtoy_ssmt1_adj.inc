subroutine da_transform_xtoy_ssmt1_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt2_adj") 

   do n=1, iv % nlocal(ssmt1)
      k = iv % ssmt1(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d_adj(jo_grad_x%t, iv%ssmt1(n)%loc, jo_grad_y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
   end do  

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt2_adj") 

end subroutine da_transform_xtoy_ssmt1_adj


