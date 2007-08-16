subroutine da_transform_xtoy_airsr (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: Does transforms from model space to AIRS locations
   !-------------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)    :: grid     ! gridded analysis increment.
   type (ob_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airsr")

   do n=iv%plocal(iv%time-1,airsr) + 1, iv%plocal(iv%time,airsr)
      k = iv % airsr(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa%t, iv%airsr(n)%loc, y%airsr(n)%t, k, iv%airsr(n)%zk, k)
      call da_interp_lin_3d(grid%xa%q, iv%airsr(n)%loc, y%airsr(n)%q, k, iv%airsr(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airsr")

end subroutine da_transform_xtoy_airsr


