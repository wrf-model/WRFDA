subroutine da_transform_xtoy_radar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   !---------------------------------------------------------------------
 
   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n                         ! Loop counter.      
   integer :: k                         ! Index dimension.   
   integer :: num_levs                  ! Number of obs levels.
   real    :: model_p(max_ob_levels)    ! Model value p at ob location.
   real    :: model_u(max_ob_levels)    ! Model value u at ob location.
   real    :: model_v(max_ob_levels)    ! Model value v at ob location.
   real    :: model_w(max_ob_levels)    ! Model value w at ob location.
   real    :: model_qrn(max_ob_levels)  ! Model qrn at ob location.
   real    :: model_qrnb(max_ob_levels) ! Model qrn at ob location.
   real    :: model_ps

   real    :: xr,yr,zr

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar")

   do n=iv%plocal(iv%time-1,radar) + 1, &
      iv%plocal(iv%time,radar)
      num_levs = iv % radar(n) % info % levels

      do k = 1, num_levs
         model_qrnb(k) = iv%radar(n)%model_qrn(k)
         model_p(k) = iv%radar(n)%model_p(k)
      end do

      model_ps   = iv%radar(n)%model_ps

      ! [1.4] Interpolate horizontally from dot points:
      call da_interp_lin_3d(grid%xa%u,   iv%radar(n)%loc, model_u,   num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa%v,   iv%radar(n)%loc, model_v,   num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa%qrn, iv%radar(n)%loc, model_qrn, num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa%wh,  iv%radar(n)%loc, model_w,   num_levs, iv%radar(n)%zk, num_levs)

      ! [1.7] Calculate rv and rf at OBS location

      xr = grid%xb%ds * (iv%radar(n)%loc%x - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds * (iv%radar(n)%loc%y - iv%radar(n)%stn_loc%y)

      do k = 1, num_levs
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

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar")

end subroutine da_transform_xtoy_radar 


