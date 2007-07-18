subroutine da_transform_xtoy_ships (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout)  :: grid
   type (ob_type),    intent(in)     :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout)  :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   if (iv%num_ships > 0) then
      !xyh y%ships(:)%u = 0.0
      !xyh y%ships(:)%v = 0.0
      !xyh y%ships(:)%t = 0.0
      !xyh y%ships(:)%p = 0.0
      !xyh y%ships(:)%q = 0.0

      if (sfc_assi_options == sfc_assi_options_1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, iv%ob_numb(iv%current_ob_time)%ships
            ! [1.1] Get horizontal interpolation weights:

            i = iv%ships(n)%loc%i
            dy = iv%ships(n)%loc%dy
            dym = iv%ships(n)%loc%dym
            j = iv%ships(n)%loc%j
            dx = iv%ships(n)%loc%dx
            dxm = iv%ships(n)%loc%dxm

            ! [1.2] Interpolate horizontally:
            call da_interp_obs_lin_2d(grid%xa % u, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%u, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % v, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%v, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % t, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%t, iv%ships(n)%zk)
            call da_interp_obs_lin_2d(grid%xa % q, i, j, dx, dy, dxm, dym, &
                                   y%ships(n)%q, iv%ships(n)%zk)

            call da_interp_lin_2d(grid%xa % psfc, ims, ime, jms, jme, &
                               i, j, dx, dy, dxm, dym, &
                               y%ships(n)%p)
         end do
      else if (sfc_assi_options == 2) then
          ! [2.0] Surface assimilation approach 2
          do n=iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
             iv%ob_numb(iv%current_ob_time)%ships
             call da_transform_xtopsfc(grid,iv%ships(n),y%ships(n))
         end do
      end if
   end if

end subroutine da_transform_xtoy_ships


