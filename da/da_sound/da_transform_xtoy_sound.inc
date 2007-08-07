subroutine da_transform_xtoy_sound (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)    :: grid
   type (ob_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.

   do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound
      k = iv % sound(n) % info % levels

      if (k < 1) cycle
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d(grid%xa % u, iv%sound(n)%loc, y%sound(n)%u, k, iv%sound(n)%zk, k)
      call da_interp_lin_3d(grid%xa % v, iv%sound(n)%loc, y%sound(n)%v, k, iv%sound(n)%zk, k)
      call da_interp_lin_3d(grid%xa % t, iv%sound(n)%loc, y%sound(n)%t, k, iv%sound(n)%zk, k)
      call da_interp_lin_3d(grid%xa % q, iv%sound(n)%loc, y%sound(n)%q, k, iv%sound(n)%zk, k)
   end do

end subroutine da_transform_xtoy_sound


