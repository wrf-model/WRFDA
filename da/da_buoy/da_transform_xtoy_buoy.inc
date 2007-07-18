subroutine da_transform_xtoy_buoy (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type), intent(inout)  :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_buoy > 0) then

      ! WHY?
      !xyh      y%buoy(:)%u = 0.0
      !xyh      y%buoy(:)%v = 0.0
      !xyh      y%buoy(:)%t = 0.0
      !xyh      y%buoy(:)%p = 0.0
      !xyh      y%buoy(:)%q = 0.0

      if (sfc_assi_options == sfc_assi_options_1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, &
            iv%ob_numb(iv%current_ob_time)%buoy

            ! [1.1] Get horizontal interpolation weights:

            i = iv%buoy(n)%loc%i
            dy = iv%buoy(n)%loc%dy
            dym = iv%buoy(n)%loc%dym
            j = iv%buoy(n)%loc%j
            dx = iv%buoy(n)%loc%dx
            dxm = iv%buoy(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d(grid%xa % u, i, j, dx, dy, dxm, dym, &
               y%buoy(n)%u, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % v, i, j, dx, dy, dxm, dym, &
               y%buoy(n)%v, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % t, i, j, dx, dy, dxm, dym, &
               y%buoy(n)%t, iv%buoy(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % q, i, j, dx, dy, dxm, dym, &
               y%buoy(n)%q, iv%buoy(n)%zk)

            call da_interp_lin_2d(grid%xa % psfc, ims, ime, jms, jme, &
               i, j, dx, dy, dxm, dym, y%buoy(n)%p)
         end do
      else if (sfc_assi_options == 2) then
         ! [2.0] Surface assimilation approach 2
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy
            call da_transform_xtopsfc(grid,iv%buoy(n),y%buoy(n))
         end do
      end if
   end if

end subroutine da_transform_xtoy_buoy


