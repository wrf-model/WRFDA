subroutine da_transform_xtoy_ssmt1 ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   

   IF ( iv%num_ssmt1 > 0 ) THEN

      do n=1, iv % num_ssmt1

       if(iv%ssmt1(n)%loc%proc_domain_with_halo) then

         y%ssmt1(n)%t(:) = 0.0

         k = iv % ssmt1(n) % info % levels

!        [1.1] Get horizontal interpolation weights:

         i = iv%ssmt1(n)%loc%i
         dy = iv%ssmt1(n)%loc%dy
         dym = iv%ssmt1(n)%loc%dym
         j = iv%ssmt1(n)%loc%j
         dx = iv%ssmt1(n)%loc%dx
         dxm = iv%ssmt1(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#else
         call Interp_lin_3D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#endif
       endif

      end do

   ENDIF

end subroutine da_transform_xtoy_ssmt1

