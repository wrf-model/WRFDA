SUBROUTINE da_transform_xtoy_metar ( xa, xb, iv, xp, y )

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
   
   IF ( iv%num_metar > 0 ) THEN

!xyh      y%metar(:)%u = 0.0
!xyh      y%metar(:)%v = 0.0
!xyh      y%metar(:)%t = 0.0
!xyh      y%metar(:)%p = 0.0
!xyh      y%metar(:)%q = 0.0

      if (sfc_assi_options == 1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar

       if(iv%metar(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%metar(n)%loc%i
         dy = iv%metar(n)%loc%dy
         dym = iv%metar(n)%loc%dym
         j = iv%metar(n)%loc%j
         dx = iv%metar(n)%loc%dx
         dxm = iv%metar(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%u, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%v, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%t, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%q, iv%metar(n)%zk)

         call Interp_lin_2D(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%metar(n)%p)
#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%u, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%v, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%t, iv%metar(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%metar(n)%q, iv%metar(n)%zk)

         call Interp_lin_2D(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%metar(n)%p)
#endif
       endif
      end do
! [2.0] Surface assimilation approach 2

      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar
            if(iv%metar(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc(xb,xa,xp,iv%metar(n),y%metar(n))
            endif
         end do
      endif
   ENDIF

END SUBROUTINE da_transform_xtoy_metar

