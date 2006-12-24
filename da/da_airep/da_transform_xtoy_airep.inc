subroutine da_transform_xtoy_airep (xa, iv, xp, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_airep > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%airep + 1, iv%ob_numb(iv%current_ob_time)%airep
         k = iv % airep(n) % info % levels
         if (k < 1) cycle

         ! xyh         y%airep(n)%u(:) = 0.0
         ! xyh         y%airep(n)%v(:) = 0.0
         ! xyh         y%airep(n)%t(:) = 0.0

         ! [1.1] Get horizontal interpolation weights:

         i = iv%airep(n)%loc%i
         dy = iv%airep(n)%loc%dy
         dym = iv%airep(n)%loc%dym
         j = iv%airep(n)%loc%j
         dx = iv%airep(n)%loc%dx
         dxm = iv%airep(n)%loc%dxm

         ![1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%u, k, iv%airep(n)%zk, k)

         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%v, k, iv%airep(n)%zk, k)

         call da_interp_lin_3d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%t, k, iv%airep(n)%zk, k)

#else
         call da_interp_lin_3d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%u(1), k, iv%airep(n)%zk(1), k)

         call da_interp_lin_3d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%v(1), k, iv%airep(n)%zk(1), k)

         call da_interp_lin_3d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                           y%airep(n)%t(1), k, iv%airep(n)%zk(1), k)

#endif
      end do
   end if

end subroutine da_transform_xtoy_airep



