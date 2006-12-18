SUBROUTINE da_transform_xtoy_airep_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_airep > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%airep + 1, iv%ob_numb(iv%current_ob_time)%airep

       if(iv%airep(n)%loc%proc_domain_with_halo) then

         k = iv % airep(n) % info % levels

         if ( k < 1 ) cycle
      
!        [1.1] Get horizontal interpolation weights:

         i = iv%airep(n)%loc%i
         dy = iv%airep(n)%loc%dy
         dym = iv%airep(n)%loc%dym
         j = iv%airep(n)%loc%j
         dx = iv%airep(n)%loc%dx
         dxm = iv%airep(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%u, k, iv%airep(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%v, k, iv%airep(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%t, k, iv%airep(n)%zk, k)
#else
         call Interp_lin_3D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%u(1), k, iv%airep(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%v(1), k, iv%airep(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%airep(n)%t(1), k, iv%airep(n)%zk(1), k)
#endif
       endif

      END DO
   
   END IF

END SUBROUTINE da_transform_xtoy_airep_adj
