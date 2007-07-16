subroutine da_transform_xtoy_pilot (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)   :: grid
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (y_type), intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_pilot > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot

         !xyh         y%pilot(n)%u(:) = 0.0
         !xyh         y%pilot(n)%v(:) = 0.0

         k = iv % pilot(n) % info % levels

         if (k < 1) cycle

         ! [1.3] Get horizontal interpolation weights:

         i = iv%pilot(n)%loc%i
         dy = iv%pilot(n)%loc%dy
         dym = iv%pilot(n)%loc%dym
         j = iv%pilot(n)%loc%j
         dx = iv%pilot(n)%loc%dx
         dxm = iv%pilot(n)%loc%dxm

         ! [1.4] Interpolate horizontally:
         call da_interp_lin_3d(grid%xa % u, i, j, dx, dy, dxm, dym, &
                           y%pilot(n)%u, k, iv%pilot(n)%zk, k)
         call da_interp_lin_3d(grid%xa % v, i, j, dx, dy, dxm, dym, &
                           y%pilot(n)%v, k, iv%pilot(n)%zk, k)
      end do
   end if

end subroutine da_transform_xtoy_pilot 


