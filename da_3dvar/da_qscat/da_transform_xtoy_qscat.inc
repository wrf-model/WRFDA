subroutine da_transform_xtoy_qscat( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_qscat > 0 ) THEN

!xyh      y%qscat(:)%u = 0.0
!xyh      y%qscat(:)%v = 0.0

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
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%v, iv%qscat(n)%zk)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%qscat(n)%v, iv%qscat(n)%zk)
#endif
       endif

      end do

   ENDIF

end subroutine da_transform_xtoy_qscat

