SUBROUTINE DA_Transform_XToY_PolarAMV_Adj ( iv, xp, jo_grad_y, jo_grad_x )

!-------------------------------------------------------------------------
!  HISTORY:
!
!      Created: 03/05/2005                   Syed RH Rizvi
!
!      Purpose: X to Y Transpose operator for Polar AMV's
!
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n, k     ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_polaramv > 0 ) THEN

      do n=1, iv % num_polaramv

       if(iv%polaramv(n)%loc%proc_domain_with_halo) then

         k = iv % polaramv(n) % info % levels

         if ( k < 1 ) cycle

         i = iv%polaramv(n)%loc%i
         dy = iv%polaramv(n)%loc%dy
         dym = iv%polaramv(n)%loc%dym
         j = iv%polaramv(n)%loc%j
         dx = iv%polaramv(n)%loc%dx
         dxm = iv%polaramv(n)%loc%dxm

#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%polaramv(n)%u, k, iv%polaramv(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%polaramv(n)%v, k, iv%polaramv(n)%zk, k)
#else
         call Interp_lin_3D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%polaramv(n)%u(1), k, iv%polaramv(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%polaramv(n)%v(1), k, iv%polaramv(n)%zk(1), k)
#endif
       endif

      END DO

   END IF

END SUBROUTINE DA_Transform_XToY_PolarAMV_Adj
