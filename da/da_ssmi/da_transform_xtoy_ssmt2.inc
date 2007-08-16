subroutine da_transform_xtoy_ssmt2 (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer  :: n  ! Loop counter.
   integer  :: k  ! Index dimension.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt2") 

   do n=1, iv % nlocal(ssmt2)
      y%ssmt2(n)%rh(:) = 0.0
      k = iv % ssmt2(n) % info % levels
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % rh, iv%ssmt2(n)%loc, y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
    end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt2") 

end subroutine da_transform_xtoy_ssmt2


