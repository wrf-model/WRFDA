subroutine da_transform_xtoy_ssmt1_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_ssmt1 > 0 ) THEN

      DO n=1, iv % num_ssmt1

       if(iv%ssmt1(n)%loc%proc_domain_with_halo) then

         k = iv % ssmt1(n) % info % levels

         if ( k < 1 ) cycle
      
!        [1.1] Get horizontal interpolation weights:

         i = iv%ssmt1(n)%loc%i
         dy = iv%ssmt1(n)%loc%dy
         dym = iv%ssmt1(n)%loc%dym
         j = iv%ssmt1(n)%loc%j
         dx = iv%ssmt1(n)%loc%dx
         dxm = iv%ssmt1(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#else
         call Interp_lin_3D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt1(n)%t, k, iv%ssmt1(n)%zk, k)
#endif
       endif

      END DO
   
   END IF

end subroutine da_transform_xtoy_ssmt1_adj

