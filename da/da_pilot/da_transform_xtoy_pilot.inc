subroutine da_transform_xtoy_pilot (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.


   do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot
      k = iv % pilot(n) % info % levels
      if (k < 1) cycle
      ! [1.4] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % u, iv%pilot(n)%loc, y%pilot(n)%u, k, iv%pilot(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%pilot(n)%loc, y%pilot(n)%v, k, iv%pilot(n)%zk, k)
   end do

end subroutine da_transform_xtoy_pilot 


