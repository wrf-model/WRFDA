subroutine da_transform_xtoy_ssmt2 ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   

   IF ( iv%num_ssmt2 > 0 ) THEN
      do n=1, iv % num_ssmt2
         y%ssmt2(n)%rh(:) = 0.0

         k = iv % ssmt2(n) % info % levels

         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt2(n)%loc%i
         dy = iv%ssmt2(n)%loc%dy
         dym = iv%ssmt2(n)%loc%dym
         j = iv%ssmt2(n)%loc%j
         dx = iv%ssmt2(n)%loc%dx
         dxm = iv%ssmt2(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa % rh, xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#else
         call da_interp_lin_3d(xa % rh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#endif
       end do
   ENDIF

end subroutine da_transform_xtoy_ssmt2

