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

   do n=iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, iv%ob_numb(iv%current_ob_time)%polaramv
      k = iv % polaramv(n) % info % levels
      if (k < 1) cycle
      call da_interp_lin_3d(grid%xa % u, iv%polaramv(n)%loc, y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%polaramv(n)%loc, y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
   end do

end subroutine da_transform_xtoy_polaramv


