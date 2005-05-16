SUBROUTINE DA_Transform_XToY_Synop ( xa, xb, iv, xp, y )

   IMPLICIT NONE

   TYPE (x_type),  INTENT(INoUT) :: xa       ! gridded analysis increment.
   TYPE (xb_type), INTENT(IN)    :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: iv       ! Innovation vector (O-B).
   type (xpose_type), INTENT(IN) :: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT)  :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
   
   IF ( iv%num_synop > 0 ) THEN

!xyh      y%synop(:)%u = 0.0
!xyh      y%synop(:)%v = 0.0
!xyh      y%synop(:)%t = 0.0
!xyh      y%synop(:)%p = 0.0
!xyh      y%synop(:)%q = 0.0

      if (sfc_assi_options == 1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop

       if(iv%synop(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%synop(n)%loc%i
         j = iv%synop(n)%loc%j
         dx = iv%synop(n)%loc%dx
         dy = iv%synop(n)%loc%dy
         dxm = iv%synop(n)%loc%dxm
         dym = iv%synop(n)%loc%dym

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%u, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%v, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%t, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%q, iv%synop(n)%zk)

         call Interp_lin_2D(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%synop(n)%p)

#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%u, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%v, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%t, iv%synop(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%synop(n)%q, iv%synop(n)%zk)

         call Interp_lin_2D(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%synop(n)%p)
#endif

       endif

      end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop
            if(iv%synop(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc( xb, xa, xp, iv%synop(n), y%synop(n) )
            endif
         enddo
      endif

   ENDIF

END SUBROUTINE DA_Transform_XToY_Synop

