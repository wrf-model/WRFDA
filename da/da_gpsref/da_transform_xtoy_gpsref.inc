subroutine da_transform_xtoy_gpsref (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)   :: grid
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (y_type), intent(inout) :: y        ! y = h (grid%xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  !
   real                         :: dy, dym  !
   integer                      :: num_levs ! obs vertical levels

   if (iv%num_gpsref < 1) return

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
      iv%ob_numb(iv%current_ob_time)%gpsref

      ! y%gpsref(n)%ref(:) = 0.0
      num_levs = iv % gpsref(n) % info % levels

      i = iv%gpsref(n)%loc%i
      j = iv%gpsref(n)%loc%j
      dx = iv%gpsref(n)%loc%dx
      dy = iv%gpsref(n)%loc%dy
      dxm = iv%gpsref(n)%loc%dxm
      dym = iv%gpsref(n)%loc%dym

      call da_interp_lin_3d(grid%xa%ref, i, j, dx, dy, dxm, dym, &
         y%gpsref(n)%ref, num_levs, iv%gpsref(n)%zk, num_levs)
   end do

end subroutine da_transform_xtoy_gpsref


