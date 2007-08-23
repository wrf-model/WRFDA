subroutine da_transform_xtoy_geoamv (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Geo. AMVs
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n ! Loop counter.
   integer :: k ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_geoamv")

   do n=iv%plocal(iv%time-1,geoamv)+1, iv%plocal(iv%time,geoamv)
      k = iv % geoamv(n) % info % levels
      if (k < 1) cycle
      ! Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % u,iv%geoamv(n)%loc, y%geoamv(n)%u, k, iv%geoamv(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%geoamv(n)%loc, y%geoamv(n)%v, k, iv%geoamv(n)%zk, k)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_geoamv")

end subroutine da_transform_xtoy_geoamv


