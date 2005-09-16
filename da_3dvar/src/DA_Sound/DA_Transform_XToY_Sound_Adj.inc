SUBROUTINE da_transform_xtoy_sound_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !


   IF ( iv%num_sound > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound
       if(iv%sound(n)%loc%proc_domain_with_halo) then
      
!        [1.1] Get horizontal interpolation weights:

         i = iv%sound(n)%loc%i
         j = iv%sound(n)%loc%j
         dx = iv%sound(n)%loc%dx
         dy = iv%sound(n)%loc%dy
         dxm = iv%sound(n)%loc%dxm
         dym = iv%sound(n)%loc%dym

         k = iv % sound(n) % info % levels

         if ( k < 1 ) cycle

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%u, k, iv%sound(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%v, k, iv%sound(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%q, k, iv%sound(n)%zk, k)
         call Interp_lin_3D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%t, k, iv%sound(n)%zk, k)
#else
         call Interp_lin_3D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%u(1), k, iv%sound(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%v(1), k, iv%sound(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%q(1), k, iv%sound(n)%zk(1), k)
         call Interp_lin_3D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 jo_grad_y%sound(n)%t(1), k, iv%sound(n)%zk(1), k)
#endif
       endif

      END DO
   
   END IF

END SUBROUTINE da_transform_xtoy_sound_adj

