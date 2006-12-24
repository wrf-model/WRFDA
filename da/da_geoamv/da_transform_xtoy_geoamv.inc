subroutine da_transform_xtoy_geoamv (xa, iv, xp, y)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Geo. AMVs
   !-------------------------------------------------------------------------

   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j , k ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_geoamv > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%geoamv+1, iv%ob_numb(iv%current_ob_time)%geoamv

         ! WHY
         !xyh         y%geoamv(n)%u(:) = 0.0
         !xyh         y%geoamv(n)%v(:) = 0.0

         k = iv % geoamv(n) % info % levels
         if (k < 1) cycle

         i = iv%geoamv(n)%loc%i
         dy = iv%geoamv(n)%loc%dy
         dym = iv%geoamv(n)%loc%dym
         j = iv%geoamv(n)%loc%j
         dx = iv%geoamv(n)%loc%dx
         dxm = iv%geoamv(n)%loc%dxm

         ! Interpolate horizontally:

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%geoamv(n)%u, k, iv%geoamv(n)%zk, k)
         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                        y%geoamv(n)%v, k, iv%geoamv(n)%zk, k)
#else
         call da_interp_lin_3d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
            dxm, dym, y%geoamv(n)%u(1), k, iv%geoamv(n)%zk(1), k)
         call da_interp_lin_3d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
            dxm, dym, y%geoamv(n)%v(1), k, iv%geoamv(n)%zk(1), k)
#endif
      end do
   end if

end subroutine da_transform_xtoy_geoamv


