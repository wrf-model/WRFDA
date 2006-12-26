subroutine da_transform_xtoy_radar (xa, xb, iv, xp, y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   !---------------------------------------------------------------------
 
   implicit none

   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)   :: xb          ! first guess state.
   type (ob_type), intent(in)   :: iv       ! Innovation vector (O-B).
   type (xpose_type), intent(in):: xp       ! Domain decomposition vars.
   type (y_type), intent(inout) :: y        ! y = h (xa) (linear)

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

   if (iv%num_Radar > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, &
         iv%ob_numb(iv%current_ob_time)%radar
         num_levs = iv % Radar(n) % info % levels

         ! [1.3] Get dot pt. horizontal interpolation weights:

         i = iv%Radar(n)%loc%i
         j = iv%Radar(n)%loc%j
         dx = iv%Radar(n)%loc%dx
         dy = iv%Radar(n)%loc%dy
         dxm = iv%Radar(n)%loc%dxm
         dym = iv%Radar(n)%loc%dym

         do k = 1, num_levs
            model_qrnb(k) = iv%Radar(n)%model_qrn(k)
            model_p(k) = iv%Radar(n)%model_p(k)
         end do

         model_ps   = iv%Radar(n)%model_ps

         ! [1.4] Interpolate horizontally from dot points:
         call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                            model_u, num_levs, iv%Radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                            model_v, num_levs, iv%Radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(xa % qrn, xp, &
                            i, j, dx, dy, dxm, dym, &
                            model_qrn, num_levs, iv%Radar(n)%zk, &
                            num_levs)
         call da_interp_lin_3d(xa%wh, xp,   &
                            i, j, dx, dy, dxm, dym, &
                            model_w, num_levs, iv%Radar(n)%zk, &
                            num_levs)

         ! [1.7] Calculate rv and rf at OBS location

         xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
         yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

         do k = 1, num_levs

            ! y%Radar(n)%rv(k) = 0.0
            ! y%Radar(n)%rf(k) = 0.0

            if (iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
                 iv % Radar(n) % height_qc(k) /= above_model_lid) then
               if (use_Radar_rv) then
                  if (iv % Radar(n) % rv(k) % qc >= obs_qc_pointer) then
                     zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                     call da_radial_velocity_lin(y%Radar(n)%rv(k), &
                        model_p(k), &
                        model_u(k), model_v(k), model_w(k), model_qrn(k),    &
                        model_ps, xr, yr, zr, model_qrnb(k))
                  end if
               end if

               if (use_Radar_rf) then
                  if (iv % Radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     call da_reflectivity_lin(y%Radar(n)%rf(k), &
                        model_qrn(k), &
                        model_qrnb(k))
                  end if
               end if
            end if
         end do
      end do
   end if

end subroutine da_transform_xtoy_radar 


