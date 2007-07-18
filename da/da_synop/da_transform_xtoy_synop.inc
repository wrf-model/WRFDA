subroutine da_transform_xtoy_synop (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_synop > 0) then

      ! WHY?
      ! xyh
      ! y%synop(:)%u = 0.0
      ! y%synop(:)%v = 0.0
      ! y%synop(:)%t = 0.0
      ! y%synop(:)%p = 0.0
      ! y%synop(:)%q = 0.0

      if (sfc_assi_options == sfc_assi_options_1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, iv%ob_numb(iv%current_ob_time)%synop

            ! [1.1] Get horizontal interpolation weights:

            i = iv%synop(n)%loc%i
            j = iv%synop(n)%loc%j
            dx = iv%synop(n)%loc%dx
            dy = iv%synop(n)%loc%dy
            dxm = iv%synop(n)%loc%dxm
            dym = iv%synop(n)%loc%dym

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d(grid%xa % u, i, j, dx, dy, dxm, dym, &
               y%synop(n)%u, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % v, i, j, dx, dy, dxm, dym, &
               y%synop(n)%v, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % t, i, j, dx, dy, dxm, dym, &
               y%synop(n)%t, iv%synop(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % q, i, j, dx, dy, dxm, dym, &
               y%synop(n)%q, iv%synop(n)%zk)

            call da_interp_lin_2d(grid%xa % psfc, ims, ime, jms, jme, &
                                i, j, dx, dy, dxm, dym, y%synop(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%synop + 1, &
            iv%ob_numb(iv%current_ob_time)%synop
            call da_transform_xtopsfc(grid, iv%synop(n), y%synop(n))
         end do
      end if
   end if

end subroutine da_transform_xtoy_synop


