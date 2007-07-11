subroutine da_transform_xtoy_radar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   !---------------------------------------------------------------------
 
   implicit none

   type (domain),  intent(in)   :: grid
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (y_type), intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   integer                      :: num_levs ! Number of obs levels.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !

   real, dimension(max_ob_levels)  :: model_p  ! Model value p at ob location.
   real, dimension(max_ob_levels)  :: model_u  ! Model value u at ob location.
   real, dimension(max_ob_levels)  :: model_v  ! Model value v at ob location.
   real, dimension(max_ob_levels)  :: model_w  ! Model value w at ob location.
   real, dimension(max_ob_levels)  :: model_qrn ! Model qrn at ob location.
   real, dimension(max_ob_levels)  :: model_qrnb! Model qrn at ob location.
   real                            :: model_ps

   real                         :: xr,yr,zr

   if (iv%num_radar > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, &
         iv%ob_numb(iv%current_ob_time)%radar
         num_levs = iv % radar(n) % info % levels

         ! [1.3] Get dot pt. horizontal interpolation weights:

         i = iv%radar(n)%loc%i
         j = iv%radar(n)%loc%j
         dx = iv%radar(n)%loc%dx
         dy = iv%radar(n)%loc%dy
         dxm = iv%radar(n)%loc%dxm
         dym = iv%radar(n)%loc%dym

         do k = 1, num_levs
            model_qrnb(k) = iv%radar(n)%model_qrn(k)
            model_p(k) = iv%radar(n)%model_p(k)
         end do

         model_ps   = iv%radar(n)%model_ps

         ! [1.4] Interpolate horizontally from dot points:
         call da_interp_lin_3d(grid%xa % u, i, j, dx, dy, dxm, dym, &
                            model_u, num_levs, iv%radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(grid%xa % v, i, j, dx, dy, dxm, dym, &
                            model_v, num_levs, iv%radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(grid%xa % qrn, &
                            i, j, dx, dy, dxm, dym, &
                            model_qrn, num_levs, iv%radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(grid%xa%wh,   &
                            i, j, dx, dy, dxm, dym, &
                            model_w, num_levs, iv%radar(n)%zk, &
                            num_levs)

         ! [1.7] Calculate rv and rf at OBS location

         xr = grid%xb%ds * (iv%radar(n)%loc%x - iv%radar(n)%stn_loc%x)
         yr = grid%xb%ds * (iv%radar(n)%loc%y - iv%radar(n)%stn_loc%y)

         do k = 1, num_levs

            ! y%radar(n)%rv(k) = 0.0
            ! y%radar(n)%rf(k) = 0.0

            if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
                 iv % radar(n) % height_qc(k) /= above_model_lid) then
               if (use_radar_rv) then
                  if (iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                     zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                     call da_radial_velocity_lin(y%radar(n)%rv(k), &
                        model_p(k), &
                        model_u(k), model_v(k), model_w(k), model_qrn(k),    &
                        model_ps, xr, yr, zr, model_qrnb(k))
                  end if
               end if

               if (use_radar_rf) then
                  if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     call da_reflectivity_lin(y%radar(n)%rf(k), &
                        model_qrn(k), &
                        model_qrnb(k))
                  end if
               end if
            end if
         end do
      end do
   end if

end subroutine da_transform_xtoy_radar 


