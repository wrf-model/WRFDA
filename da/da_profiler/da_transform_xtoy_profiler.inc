subroutine da_transform_xtoy_profiler (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_profiler")
   
   do n=iv%plocal(iv%time-1,profiler) + 1, iv%plocal(iv%time,profiler)
      k = iv % profiler(n) % info % levels
      if (k < 1) cycle
      ! [1.4] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % u, iv%profiler(n)%loc, y%profiler(n)%u, k, iv%profiler(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%profiler(n)%loc, y%profiler(n)%v, k, iv%profiler(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_profiler")

end subroutine da_transform_xtoy_profiler 


