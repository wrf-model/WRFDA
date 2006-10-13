subroutine da_transform_xtoy_airsr ( xa, iv, xp, y )

   !-------------------------------------------------------------------------
   ! Purpose: Does transforms from model space to AIRS locations
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type),     INTENT(IN)    :: xa       ! gridded analysis increment.
   TYPE (ob_type),    INTENT(IN)    :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN)    :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !

   IF ( iv%num_airsr > 0 ) THEN
      do n=iv%ob_numb(iv%current_ob_time-1)%airsr + 1, iv%ob_numb(iv%current_ob_time)%airsr

         ! [1.1] Get horizontal interpolation weights:

         i = iv%airsr(n)%loc%i
         j = iv%airsr(n)%loc%j
         dx = iv%airsr(n)%loc%dx
         dy = iv%airsr(n)%loc%dy
         dxm = iv%airsr(n)%loc%dxm
         dym = iv%airsr(n)%loc%dym

         k = iv % airsr(n) % info % levels

         if ( k < 1 ) cycle

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                           y%airsr(n)%t, k, iv%airsr(n)%zk, k)
         call da_interp_lin_3d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                           y%airsr(n)%q, k, iv%airsr(n)%zk, k)
#else
         call da_interp_lin_3d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                           y%airsr(n)%t(1), k, iv%airsr(n)%zk(1), k)
         call da_interp_lin_3d(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                           y%airsr(n)%q(1), k, iv%airsr(n)%zk(1), k)
#endif
      end do
   ENDIF

end subroutine da_transform_xtoy_airsr

