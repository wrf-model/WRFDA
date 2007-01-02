SUBROUTINE DA_Transform_XToY_Ships ( xa, xb, iv, xp, y )

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
   
   IF ( iv%num_ships > 0 ) THEN

!xyh      y%ships(:)%u = 0.0
!xyh      y%ships(:)%v = 0.0
!xyh      y%ships(:)%t = 0.0
!xyh      y%ships(:)%p = 0.0
!xyh      y%ships(:)%q = 0.0

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
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%u, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%v, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%t, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%q, iv%ships(n)%zk)

         call Interp_lin_2D(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, &
                            y%ships(n)%p)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%u, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%v, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%t, iv%ships(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%ships(n)%q, iv%ships(n)%zk)

         call Interp_lin_2D(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%ships(n)%p)
#endif
       endif

      end do
! [2.0] Surface assimilation approach 2

      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
            if(iv%ships(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc(xb,xa,xp,iv%ships(n),y%ships(n))
            endif
         end do
      endif
   ENDIF

END SUBROUTINE DA_Transform_XToY_Ships

