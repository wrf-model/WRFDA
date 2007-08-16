subroutine da_transform_xtoy_polaramv (grid, iv, y)

   !----------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Polar AMV's               
   !----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n ! Loop counter.
   integer :: k ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_polaramv")

   do n=iv%plocal(iv%time-1,polaramv) + 1, iv%plocal(iv%time,polaramv)
      k = iv % polaramv(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d(grid%xa % u, iv%polaramv(n)%loc, y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%polaramv(n)%loc, y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_polaramv")

end subroutine da_transform_xtoy_polaramv


