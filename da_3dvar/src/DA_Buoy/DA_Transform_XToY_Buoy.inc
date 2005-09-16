SUBROUTINE DA_Transform_XToY_Buoy ( xa, xb, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(INOUT) :: xa       ! gridded analysis increment.
   TYPE (xb_type), INTENT(IN)    :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN) :: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT)  :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_buoy > 0 ) THEN

!xyh      y%buoy(:)%u = 0.0
!xyh      y%buoy(:)%v = 0.0
!xyh      y%buoy(:)%t = 0.0
!xyh      y%buoy(:)%p = 0.0
!xyh      y%buoy(:)%q = 0.0

      if (sfc_assi_options == 1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy

       if(iv%buoy(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%buoy(n)%loc%i
         dy = iv%buoy(n)%loc%dy
         dym = iv%buoy(n)%loc%dym
         j = iv%buoy(n)%loc%j
         dx = iv%buoy(n)%loc%dx
         dxm = iv%buoy(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%u, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%v, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%t, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%q, iv%buoy(n)%zk)

         call Interp_lin_2D(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            y%buoy(n)%p)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%u, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%v, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%t, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%buoy(n)%q, iv%buoy(n)%zk)

         call Interp_lin_2D(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%buoy(n)%p)
#endif
       endif

      end do
! [2.0] Surface assimilation approach 2

      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy
            if(iv%buoy(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc(xb,xa,xp,iv%buoy(n),y%buoy(n))
            endif
         end do
      endif
   ENDIF

END SUBROUTINE DA_Transform_XToY_Buoy

