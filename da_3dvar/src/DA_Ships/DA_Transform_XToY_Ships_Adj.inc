SUBROUTINE da_transform_xtoy_ships_adj( xb, iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(INOUT) :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, kz ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   integer  ::  n2d
   n2d =(xp%ime-xp%ims+1)*(xp%jme-xp%jms+1)
   IF ( iv%num_ships > 0 ) THEN

      if (sfc_assi_options == 1) then
       do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
      
       if(iv%ships(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%ships(n)%loc%i
         dy = iv%ships(n)%loc%dy
         dym = iv%ships(n)%loc%dym
         j = iv%ships(n)%loc%j
         dx = iv%ships(n)%loc%dx
         dxm = iv%ships(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%u, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%v, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%t, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%q, iv%ships(n)%zk)

         call Interp_lin_2D_adj(jo_grad_x % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                   i, j, dx, dy, dxm, dym, jo_grad_y%ships(n)%p)
#else
         call Interp_Obs_lin_2D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%u, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%v, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%t, iv%ships(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%ships(n)%q, iv%ships(n)%zk)

         call Interp_lin_2D_adj(jo_grad_x % psfc(xp%ims,xp%jms), &
                                xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ships(n)%p)
#endif
       endif

       END DO
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
            if(iv%ships(n)%loc%proc_domain_with_halo) then

               CALL DA_Transform_XToPsfc_Adj(xb,xp,iv%ships(n), &
                                             jo_grad_y%ships(n),jo_grad_x)
            endif
         ENDDO
      endif
   
   END IF

END SUBROUTINE da_transform_xtoy_ships_adj

