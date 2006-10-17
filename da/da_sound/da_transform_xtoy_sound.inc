subroutine da_transform_xtoy_sound ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),     INTENT(IN)    :: xa       ! gridded analysis increment.
   TYPE (ob_type),    INTENT(IN)    :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN)    :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !


   IF ( iv%num_sound > 0 ) THEN
      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

         ! [1.1] Get horizontal interpolation weights:

         i = iv%sound(n)%loc%i
         j = iv%sound(n)%loc%j
         dx = iv%sound(n)%loc%dx
         dy = iv%sound(n)%loc%dy
         dxm = iv%sound(n)%loc%dxm
         dym = iv%sound(n)%loc%dym

         !xyh y%sound(n)%u(:) = 0.0
         !xyh y%sound(n)%v(:) = 0.0
         !xyh y%sound(n)%t(:) = 0.0
         !xyh y%sound(n)%q(:) = 0.0

         k = iv % sound(n) % info % levels

         if ( k < 1 ) cycle

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%u, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%v, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%t, k, iv%sound(n)%zk, k)
         call da_interp_lin_3d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%q, k, iv%sound(n)%zk, k)
#else
         call da_interp_lin_3d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%u(1), k, iv%sound(n)%zk(1), k)
         call da_interp_lin_3d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%v(1), k, iv%sound(n)%zk(1), k)
         call da_interp_lin_3d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%t(1), k, iv%sound(n)%zk(1), k)
         call da_interp_lin_3d(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%sound(n)%q(1), k, iv%sound(n)%zk(1), k)
#endif
      end do
   ENDIF

end subroutine da_transform_xtoy_sound

