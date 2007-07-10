subroutine da_transform_xtoy_polaramv (grid, iv, y)

   !----------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Polar AMV's               
   !----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)   :: grid
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (y_type), intent(inout) :: y        ! y = h (grid%xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j , k ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   
   if (iv%num_polaramv > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, &
         iv%ob_numb(iv%current_ob_time)%polaramv
         ! xyh
         ! y%polaramv(n)%u(:) = 0.0
         ! y%polaramv(n)%v(:) = 0.0

         k = iv % polaramv(n) % info % levels
         if (k < 1) cycle

         i = iv%polaramv(n)%loc%i
         dy = iv%polaramv(n)%loc%dy
         dym = iv%polaramv(n)%loc%dym
         j = iv%polaramv(n)%loc%j
         dx = iv%polaramv(n)%loc%dx
         dxm = iv%polaramv(n)%loc%dxm

         ! Interpolate horizontally:

         call da_interp_lin_3d(grid%xa % u, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
         call da_interp_lin_3d(grid%xa % v, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
      end do
   end if

end subroutine da_transform_xtoy_polaramv


