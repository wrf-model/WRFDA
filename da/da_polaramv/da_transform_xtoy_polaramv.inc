subroutine da_transform_xtoy_polaramv ( xa, iv, xp, y )

   !----------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Polar AMV's               
   !----------------------------------------------------------------------

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j , k ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   
   if ( iv%num_polaramv > 0 ) then
!xyh      do n=1, iv % num_polaramv
      do n=iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, iv%ob_numb(iv%current_ob_time)%polaramv
!xyh         y%polaramv(n)%u(:) = 0.0
!xyh         y%polaramv(n)%v(:) = 0.0

         k = iv % polaramv(n) % info % levels
         if ( k < 1 ) cycle

         i = iv%polaramv(n)%loc%i
         dy = iv%polaramv(n)%loc%dy
         dym = iv%polaramv(n)%loc%dym
         j = iv%polaramv(n)%loc%j
         dx = iv%polaramv(n)%loc%dx
         dxm = iv%polaramv(n)%loc%dxm

         ! Interpolate horizontally:

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
#else
         call da_interp_lin_3d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%u(1), k, iv%polaramv(n)%zk(1), k)
         call da_interp_lin_3d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%polaramv(n)%v(1), k, iv%polaramv(n)%zk(1), k)
#endif
      end do
   end if

end subroutine da_transform_xtoy_polaramv


