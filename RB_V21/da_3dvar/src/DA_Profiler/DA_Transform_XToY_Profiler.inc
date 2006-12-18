SUBROUTINE da_transform_xtoy_profiler ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_profiler > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%profiler + 1, iv%ob_numb(iv%current_ob_time)%profiler

       if(iv%profiler(n)%loc%proc_domain_with_halo) then

!xyh         y%profiler(n)%u(:) = 0.0
!xyh         y%profiler(n)%v(:) = 0.0

         k = iv % profiler(n) % info % levels
 
         if ( k < 1 ) cycle

!        [1.3] Get horizontal interpolation weights:

         i = iv%profiler(n)%loc%i
         dy = iv%profiler(n)%loc%dy
         dym = iv%profiler(n)%loc%dym
         j = iv%profiler(n)%loc%j
         dx = iv%profiler(n)%loc%dx
         dxm = iv%profiler(n)%loc%dxm

!        [1.4] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%u, k, iv%profiler(n)%zk, k)
         call Interp_lin_3D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%v, k, iv%profiler(n)%zk, k)
#else
         call Interp_lin_3D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%u(1), k, iv%profiler(n)%zk(1), k)
         call Interp_lin_3D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%profiler(n)%v(1), k, iv%profiler(n)%zk(1), k)
#endif
       endif

      end do

   ENDIF

END SUBROUTINE da_transform_xtoy_profiler 

