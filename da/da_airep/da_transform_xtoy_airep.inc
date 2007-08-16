subroutine da_transform_xtoy_airep (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airep")

   do n=iv%plocal(iv%time-1,airep) + 1, iv%plocal(iv%time,airep)
      k = iv % airep(n) % info % levels
      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % u, iv%airep(n)%loc, y%airep(n)%u, k, iv%airep(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%airep(n)%loc, y%airep(n)%v, k, iv%airep(n)%zk, k)
      call da_interp_lin_3d(grid%xa % t, iv%airep(n)%loc, y%airep(n)%t, k, iv%airep(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airep")

end subroutine da_transform_xtoy_airep


