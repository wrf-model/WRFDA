subroutine da_transform_xtoy_profiler (xa, iv, xp, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   
   if (iv%num_profiler > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%profiler + 1, &
         iv%ob_numb(iv%current_ob_time)%profiler
         ! xyh
         ! y%profiler(n)%u(:) = 0.0
         ! y%profiler(n)%v(:) = 0.0

         k = iv % profiler(n) % info % levels
 
         if (k < 1) cycle

         ! [1.3] Get horizontal interpolation weights:

         i = iv%profiler(n)%loc%i
         dy = iv%profiler(n)%loc%dy
         dym = iv%profiler(n)%loc%dym
         j = iv%profiler(n)%loc%j
         dx = iv%profiler(n)%loc%dx
         dxm = iv%profiler(n)%loc%dxm

         ! [1.4] Interpolate horizontally:
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%u, k, iv%profiler(n)%zk, k)
         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%v, k, iv%profiler(n)%zk, k)
      end do
   end if

end subroutine da_transform_xtoy_profiler 


