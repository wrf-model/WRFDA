SUBROUTINE da_transform_xtoy_pseudo( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_pseudo > 0 ) THEN

      y%pseudo(:)%u = 0.0
      y%pseudo(:)%v = 0.0
      y%pseudo(:)%t = 0.0
      y%pseudo(:)%p = 0.0
      y%pseudo(:)%q = 0.0

      do n=1, iv % num_pseudo

       if(iv%pseudo(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%pseudo(n)%loc%i
         dy = iv%pseudo(n)%loc%dy
         dym = iv%pseudo(n)%loc%dym
         j = iv%pseudo(n)%loc%j
         dx = iv%pseudo(n)%loc%dx
         dxm = iv%pseudo(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%u, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%v, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%t, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % p, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%p, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%q, iv%pseudo(n)%zk)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%u, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%v, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%t, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % p(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%p, iv%pseudo(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%pseudo(n)%q, iv%pseudo(n)%zk)
#endif
       endif

      end do

   ENDIF

END SUBROUTINE da_transform_xtoy_pseudo

