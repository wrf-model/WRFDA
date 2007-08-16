subroutine da_calculate_residual(iv, y, re)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals
   !-----------------------------------------------------------------------

   implicit none
      
   type (ob_type), intent(in)        :: iv     ! Innovation vector (O-B).
   type (y_type),  intent(in)        :: y      ! y = H (xa)
   type (y_type),  intent(inout)     :: re     ! Residual (O-A).

   integer    :: np_available, np_obs_used, np_missing, np_bad_data 

   if (trace_use) call da_trace_entry("da_calculate_residual")
      
   np_available = 0
   np_obs_used  = 0
   np_missing   = 0
   np_bad_data  = 0

   !-------------------------------------------------------------------------
   ! [1.0] (O-A) = (O-B) - H x~:
   !-------------------------------------------------------------------------

   if (iv%nlocal(sound) > 0) then
      call da_residual_sound(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
      call da_residual_sonde_sfc(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
   end if

   if (iv%nlocal(synop) > 0) &
      call da_residual_synop(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(geoamv) > 0) &
      call da_residual_geoamv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(polaramv) > 0) &
      call da_residual_polaramv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(airep) > 0) &
      call da_residual_airep(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(metar) > 0) &
      call da_residual_metar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(ships) > 0) &
      call da_residual_ships(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(gpspw) > 0) &
      call da_residual_gpspw(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(gpsref) > 0) &
      call da_residual_gpsref(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(ssmi_tb) > 0) &
      call da_residual_ssmi_tb(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(ssmi_rv) > 0) &
      call da_residual_ssmi_rv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if ( iv%nlocal(ssmt1) > 0) &
      call da_residual_ssmt1(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(ssmt2) > 0) &
      call da_residual_ssmt2(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(pilot) > 0) &
      call da_residual_pilot(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(bogus) > 0) &
      call da_residual_bogus(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(satem) > 0) &
      call da_residual_satem(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (num_pseudo > 0) &
      call da_residual_pseudo(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(qscat) > 0) &
      call da_residual_qscat(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(radar) > 0) &
      call da_residual_radar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(profiler) > 0) &
      call da_residual_profiler(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(buoy) > 0) &
      call da_residual_buoy(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%num_inst > 0) &
      call da_residual_rad(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%nlocal(airsr) > 0) &
      call da_residual_airsr(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (trace_use) call da_trace_exit("da_calculate_residual")

end subroutine da_calculate_residual


