SUBROUTINE DA_Transform_XToY_Satob ( xa, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_satob > 0 ) THEN

!xyh      y%satob(:)%u = 0.0
!xyh      y%satob(:)%v = 0.0

      do n=iv%ob_numb(iv%current_ob_time-1)%satob + 1, iv%ob_numb(iv%current_ob_time)%satob

       if(iv%satob(n)%loc%proc_domain_with_halo) then

         i = iv%satob(n)%loc%i
         dy = iv%satob(n)%loc%dy
         dym = iv%satob(n)%loc%dym
         j = iv%satob(n)%loc%j
         dx = iv%satob(n)%loc%dx
         dxm = iv%satob(n)%loc%dxm

#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%satob(n)%u, iv%satob(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%satob(n)%v, iv%satob(n)%zk)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%satob(n)%u, iv%satob(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%satob(n)%v, iv%satob(n)%zk)
#endif
       endif

      end do

   ENDIF

END SUBROUTINE DA_Transform_XToY_Satob

