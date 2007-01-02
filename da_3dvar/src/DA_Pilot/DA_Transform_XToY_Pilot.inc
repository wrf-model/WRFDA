SUBROUTINE da_transform_xtoy_pilot ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_pilot > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot

       if(iv%pilot(n)%loc%proc_domain_with_halo) then

!xyh         y%pilot(n)%u(:) = 0.0
!xyh         y%pilot(n)%v(:) = 0.0

         k = iv % pilot(n) % info % levels
 
         if ( k < 1 ) cycle

!        [1.3] Get horizontal interpolation weights:

         i = iv%pilot(n)%loc%i
         dy = iv%pilot(n)%loc%dy
         dym = iv%pilot(n)%loc%dym
         j = iv%pilot(n)%loc%j
         dx = iv%pilot(n)%loc%dx
         dxm = iv%pilot(n)%loc%dxm

!        [1.4] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%pilot(n)%u, k, iv%pilot(n)%zk, k)
         call Interp_lin_3D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%pilot(n)%v, k, iv%pilot(n)%zk, k)
#else
         call Interp_lin_3D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%pilot(n)%u(1), k, iv%pilot(n)%zk(1), k)
         call Interp_lin_3D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%pilot(n)%v(1), k, iv%pilot(n)%zk(1), k)
#endif
       endif

      end do

   ENDIF

END SUBROUTINE da_transform_xtoy_pilot 

