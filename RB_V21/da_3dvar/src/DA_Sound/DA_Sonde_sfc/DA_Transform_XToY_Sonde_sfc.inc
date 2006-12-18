SUBROUTINE DA_Transform_XToY_Sonde_sfc ( xa, xb, iv, xp, y )

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
   
   IF ( iv%num_sound > 0 ) THEN

!xyh      y%sonde_sfc(:)%u = 0.0
!xyh      y%sonde_sfc(:)%v = 0.0
!xyh      y%sonde_sfc(:)%t = 0.0
!xyh      y%sonde_sfc(:)%p = 0.0
!xyh      y%sonde_sfc(:)%q = 0.0

      if (sfc_assi_options == 1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

       if(iv%sonde_sfc(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%sonde_sfc(n)%loc%i
         j = iv%sonde_sfc(n)%loc%j
         dx = iv%sonde_sfc(n)%loc%dx
         dy = iv%sonde_sfc(n)%loc%dy
         dxm = iv%sonde_sfc(n)%loc%dxm
         dym = iv%sonde_sfc(n)%loc%dym

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

         call Interp_lin_2D(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%sonde_sfc(n)%p)

#else
         call Interp_Obs_lin_2D(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
         call Interp_Obs_lin_2D(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

         call Interp_lin_2D(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                            i, j, dx, dy, dxm, dym, y%sonde_sfc(n)%p)
#endif

       endif

      end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound
            if(iv%sonde_sfc(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc( xb, xa, xp, iv%sonde_sfc(n), y%sonde_sfc(n) )
            endif
         enddo
      endif

   ENDIF

END SUBROUTINE DA_Transform_XToY_Sonde_sfc

