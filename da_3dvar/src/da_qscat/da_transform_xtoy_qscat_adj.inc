subroutine da_transform_xtoy_qscat_adj( iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)    :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_qscat > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat
      
       if(iv%qscat(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%qscat(n)%loc%i
         dy = iv%qscat(n)%loc%dy
         dym = iv%qscat(n)%loc%dym
         j = iv%qscat(n)%loc%j
         dx = iv%qscat(n)%loc%dx
         dxm = iv%qscat(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%v, iv%qscat(n)%zk)
#else
         call Interp_Obs_lin_2D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%qscat(n)%v, iv%qscat(n)%zk)
#endif
       endif

      END DO
   
   END IF

end subroutine da_transform_xtoy_qscat_adj

