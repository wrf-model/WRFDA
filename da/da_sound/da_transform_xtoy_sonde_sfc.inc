subroutine da_transform_xtoy_sonde_sfc (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout) :: grid
   type (ob_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_sound > 0) then
      ! xyh
      ! y%sonde_sfc(:)%u = 0.0
      ! y%sonde_sfc(:)%v = 0.0
      ! y%sonde_sfc(:)%t = 0.0
      ! y%sonde_sfc(:)%p = 0.0
      ! y%sonde_sfc(:)%q = 0.0

      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

            ! [1.1] Get horizontal interpolation weights:

            i = iv%sonde_sfc(n)%loc%i
            j = iv%sonde_sfc(n)%loc%j
            dx = iv%sonde_sfc(n)%loc%dx
            dy = iv%sonde_sfc(n)%loc%dy
            dxm = iv%sonde_sfc(n)%loc%dxm
            dym = iv%sonde_sfc(n)%loc%dym

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d(grid%xa % u, i, j, dx, dy, dxm, dym, &
                                   y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % v, i, j, dx, dy, dxm, dym, &
                                   y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % t, i, j, dx, dy, dxm, dym, &
                                   y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % q, i, j, dx, dy, dxm, dym, &
                                   y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

            call da_interp_lin_2d(grid%xa % psfc, ims, ime, jms, jme, &
                               i, j, dx, dy, dxm, dym, y%sonde_sfc(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
            iv%ob_numb(iv%current_ob_time)%sound
            call da_transform_xtopsfc(grid, iv%sonde_sfc(n), y%sonde_sfc(n))
         end do
     end if
  end if

end subroutine da_transform_xtoy_sonde_sfc


