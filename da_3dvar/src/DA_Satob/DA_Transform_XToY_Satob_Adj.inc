SUBROUTINE DA_Transform_XToY_Satob_Adj ( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_satob > 0 ) THEN

      do n=1, iv % num_satob

       if(iv%satob(n)%loc%proc_domain_with_halo) then

         i = iv%satob(n)%loc%i
         dy = iv%satob(n)%loc%dy
         dym = iv%satob(n)%loc%dym
         j = iv%satob(n)%loc%j
         dx = iv%satob(n)%loc%dx
         dxm = iv%satob(n)%loc%dxm
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%satob(n)%u, iv%satob(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%satob(n)%v, iv%satob(n)%zk)
#else
         call Interp_Obs_lin_2D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%satob(n)%u, iv%satob(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%satob(n)%v, iv%satob(n)%zk)
#endif
       endif

      end do

   END IF

END SUBROUTINE DA_Transform_XToY_Satob_Adj

