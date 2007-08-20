subroutine da_transform_xtoy_ssmt1 (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt1") 

   do n=1, iv % nlocal(ssmt1)
      y%ssmt1(n)%t(:) = 0.0
      k = iv % ssmt1(n) % info % levels
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % t, iv%ssmt1(n)%loc, y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt1")

end subroutine da_transform_xtoy_ssmt1


