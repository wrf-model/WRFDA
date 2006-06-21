subroutine da_transform_xtoy_polaramv ( xa, iv, xp, y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: X to Y Transform operator for Polar AMV's
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j , k ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_polaramv > 0 ) THEN


      do n=1, iv % num_polaramv

       if(iv%polaramv(n)%loc%proc_domain_with_halo) then

          y%polaramv(n)%u(:) = 0.0
          y%polaramv(n)%v(:) = 0.0

       k = iv % polaramv(n) % info % levels
         if ( k < 1 ) cycle

         i = iv%polaramv(n)%loc%i
         dy = iv%polaramv(n)%loc%dy
         dym = iv%polaramv(n)%loc%dym
         j = iv%polaramv(n)%loc%j
         dx = iv%polaramv(n)%loc%dx
         dxm = iv%polaramv(n)%loc%dxm

!   Interpolate horizontally:

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

       endif

      end do

   ENDIF

end subroutine da_transform_xtoy_polaramv
