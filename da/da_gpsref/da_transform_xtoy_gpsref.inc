subroutine da_transform_xtoy_gpsref (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! Loop counter.
   integer :: num_levs ! obs vertical levels

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
      iv%ob_numb(iv%current_ob_time)%gpsref
      num_levs = iv % gpsref(n) % info % levels
      call da_interp_lin_3d(grid%xa%ref, iv%gpsref(n)%loc, y%gpsref(n)%ref, &
         num_levs, iv%gpsref(n)%zk, num_levs)
   end do

end subroutine da_transform_xtoy_gpsref


