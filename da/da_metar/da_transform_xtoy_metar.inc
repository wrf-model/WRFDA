subroutine da_transform_xtoy_metar ( xa, xb, iv, xp, y )

   implicit none

   type (x_type),  intent(inout) :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)    :: xb       ! first guess state.
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in) :: xp       ! Domain decomposition vars.
   type (y_type), intent(inout)  :: y        ! y = h (xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (trace_use) call da_trace_entry("da_transform_xtoy_metar")

   if ( iv%num_metar > 0 ) then

      !xyh      y%metar(:)%u = 0.0
      !xyh      y%metar(:)%v = 0.0
      !xyh      y%metar(:)%t = 0.0
      !xyh      y%metar(:)%p = 0.0
      !xyh      y%metar(:)%q = 0.0

      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar

            ! [1.1] Get horizontal interpolation weights:

            i = iv%metar(n)%loc%i
            dy = iv%metar(n)%loc%dy
            dym = iv%metar(n)%loc%dym
            j = iv%metar(n)%loc%j
            dx = iv%metar(n)%loc%dx
            dxm = iv%metar(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%u, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%v, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % t, xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%t, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % q, xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%q, iv%metar(n)%zk)

            call da_interp_lin_2d(xa % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                               i, j, dx, dy, dxm, dym, y%metar(n)%p)
#else
            call da_interp_obs_lin_2d(xa % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%u, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%v, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%t, iv%metar(n)%zk)
            call da_interp_obs_lin_2d(xa % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                   y%metar(n)%q, iv%metar(n)%zk)

            call da_interp_lin_2d(xa % psfc(xp%ims,xp%jms), xp%ims, xp%ime, xp%jms, xp%jme, &
                               i, j, dx, dy, dxm, dym, y%metar(n)%p)
#endif
         end do
      else if (sfc_assi_options == 2) then
         ! [2.0] Surface assimilation approach 2
         do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar
            call da_transform_xtopsfc(xb,xa,xp,iv%metar(n),y%metar(n))
         end do
      end if
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_metar")

end subroutine da_transform_xtoy_metar

