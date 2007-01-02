SUBROUTINE da_transform_xtoy_bogus_adj( xb, iv, xp, jo_grad_y, jo_grad_x )

!---------------------------------------------------------------------
!  This subroutine is the adjoint of bogus observation operators.
!  It includes slp, winds.          Ying Zhang, September 2003.
!  Port the algorithm to WRF frame. Xiaoyan Zhang and Qingnong Xiao, February 2005.
!  Modified to be standard structure by Y.-R. Guo   06/21/2005
!---------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)     :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN)  :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(INOUT)  :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT)  :: jo_grad_x   ! grad_x(jo)
 
   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs
   REAL                           :: dx, dxm  ! 
   REAL                           :: dy, dym  !

   REAL, DIMENSION(xp%kms:xp%kme) :: model_t,tt
   REAL, DIMENSION(xp%kms:xp%kme) :: model_q,qq
   REAL, DIMENSION(xp%kms:xp%kme) :: model_p_c,pp
   REAL                           :: psfc_loc,terr_loc,ppsfc                     

   IF ( iv%num_bogus > 0 ) THEN

      DO n=1, iv % num_bogus

       if(iv%bogus(n)%loc%proc_domain_with_halo) then      

         num_levs = iv % bogus(n) % info % levels

         if ( num_levs < 1 ) cycle

!        [1.1] Get cross pt. horizontal interpolation weights:

         i = iv%bogus(n)%loc%i
         dy = iv%bogus(n)%loc%dy
         dym = iv%bogus(n)%loc%dym
         j = iv%bogus(n)%loc%j
         dx = iv%bogus(n)%loc%dx
         dxm = iv%bogus(n)%loc%dxm


!        [1.1] Adjoint feedback from Y to X for u and v:

#ifndef DEREF_KLUDGE
         call Interp_lin_3D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%u, num_levs,            &
                                iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%v, num_levs,            &
                                iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%t, num_levs,            &
                                iv%bogus(n)%zk, num_levs)
         call Interp_lin_3D_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%q, num_levs,            &
                                iv%bogus(n)%zk, num_levs)
#else
         call Interp_lin_3D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), &
                                xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%u(1), num_levs, &
                                iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), &
                                xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%v(1), num_levs, &
                                iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), &
                                xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%t(1), num_levs, &
                                iv%bogus(n)%zk(1), num_levs)
         call Interp_lin_3D_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), &
                                xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%bogus(n)%q(1), num_levs, &
                                iv%bogus(n)%zk(1), num_levs)
#endif

!        [1.2] Compute the feedback from SLP to t and q:

!         1.2.1 Background at the observation location:

         do k = xp%kts, xp%kte
            model_t(k) = dym*(dxm*xb%t(i,j,k) + dx*xb%t(i+1,j,k)) &
                          + dy *(dxm*xb%t(i,j+1,k) + dx*xb%t(i+1,j+1,k))
            model_q(k) = dym*(dxm*xb%q(i,j,k) + dx*xb%q(i+1,j,k)) &
                          + dy *(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
            model_p_c(k) = dym*(dxm*xb%p(i,j,k) + dx*xb%p(i+1,j,k)) &
                          + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo

         terr_loc = dym*(dxm*xb%terr(i,j)   + dx*xb%terr(i+1,j)) &
                    + dy *(dxm*xb%terr(i,j+1) + dx*xb%terr(i+1,j+1))
         psfc_loc = dym*(dxm*xb%psfc(i,j)   + dx*xb%psfc(i+1,j)) &
                    + dy *(dxm*xb%psfc(i,j+1) + dx*xb%psfc(i+1,j+1))

!         1.2.2 Compute the feedback from SLP to p, t, q, and psfc 
!               at the observed location:
         
         call DA_TPQ_To_SLP_Adj(model_t, model_q ,model_p_c,    &
                               terr_loc,  psfc_loc,   &
                               tt, qq, pp, ppsfc,     &
                               jo_grad_y%bogus(n)%slp, xp)  

!         1.2.3 feedback from the observed location to grid space:

!         1.2.3.1 for Psfc

         jo_grad_x % psfc(i,j) = jo_grad_x % psfc(i,j) + dym*dxm*ppsfc
         jo_grad_x % psfc(i+1,j) = jo_grad_x % psfc(i+1,j)+ dym*dx*ppsfc
         jo_grad_x % psfc(i,j+1) = jo_grad_x % psfc(i,j+1)+ dy*dxm*ppsfc
         jo_grad_x % psfc(i+1,j+1)=jo_grad_x % psfc(i+1,j+1)+dy*dx*ppsfc

!         1.2.3.2 for t, q, p

         do k = xp%kts, xp%kte
            jo_grad_x % t(i,j,k) = jo_grad_x % t(i,j,k)+dym*dxm*tt(k)
            jo_grad_x % t(i+1,j,k) = jo_grad_x % t(i+1,j,k)+ dym*dx*tt(k)
            jo_grad_x % t(i,j+1,k) = jo_grad_x % t(i,j+1,k)+ dy*dxm*tt(k)
            jo_grad_x % t(i+1,j+1,k)=jo_grad_x % t(i+1,j+1,k)+dy*dx*tt(k)
            jo_grad_x % q(i,j,k) = jo_grad_x % q(i,j,k)+dym*dxm*qq(k)
            jo_grad_x % q(i+1,j,k) = jo_grad_x % q(i+1,j,k)+ dym*dx*qq(k)
            jo_grad_x % q(i,j+1,k) = jo_grad_x % q(i,j+1,k)+ dy*dxm*qq(k)
            jo_grad_x % q(i+1,j+1,k)=jo_grad_x % q(i+1,j+1,k)+dy*dx*qq(k)
            jo_grad_x % p(i,j,k) = jo_grad_x % p(i,j,k)+dym*dxm*pp(k)
            jo_grad_x % p(i+1,j,k) = jo_grad_x % p(i+1,j,k)+ dym*dx*pp(k)
            jo_grad_x % p(i,j+1,k) = jo_grad_x % p(i,j+1,k)+ dy*dxm*pp(k)
            jo_grad_x % p(i+1,j+1,k)=jo_grad_x % p(i+1,j+1,k)+dy*dx*pp(k)
         enddo

       endif

      END DO
   
   END IF

END SUBROUTINE da_transform_xtoy_bogus_adj

