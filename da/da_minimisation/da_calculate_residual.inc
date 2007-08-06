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

   if (use_soundobs .and. iv%num_sound > 0) then
      call da_residual_sound(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
      call da_residual_sonde_sfc(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
   end if

   if (use_synopobs .and. iv%num_synop > 0) &
      call da_residual_synop(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_geoamvobs .and. iv%num_geoamv > 0) &
      call da_residual_geoamv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_polaramvobs .and. iv%num_polaramv > 0) &
      call da_residual_polaramv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_airepobs .and. iv%num_airep > 0) &
      call da_residual_airep(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_metarobs .and. iv%num_metar > 0) &
      call da_residual_metar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_shipsobs .and. iv%num_ships > 0) &
      call da_residual_ships(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_gpspwobs .and. iv%num_gpspw > 0) &
      call da_residual_gpspw(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_gpsrefobs .and. iv%num_gpsref > 0) &
      call da_residual_gpsref(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_ssmiretrievalobs .or. use_ssmitbobs) &
      call da_residual_ssmi(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_ssmt1obs .and. iv%num_ssmt1 > 0) &
      call da_residual_ssmt1(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_ssmt2obs .and. iv%num_ssmt2 > 0) &
      call da_residual_ssmt2(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_pilotobs .and. iv%num_pilot > 0) &
      call da_residual_pilot(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_bogusobs .and. iv%num_bogus > 0) &
      call da_residual_bogus(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_satemobs .and. iv%num_satem > 0) &
      call da_residual_satem(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (num_pseudo > 0) &
      call da_residual_pseudo(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_qscatobs .and. iv%num_qscat > 0) &
      call da_residual_qscat(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_radarobs .and. iv%num_radar > 0) &
      call da_residual_radar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_profilerobs .and. iv%num_profiler > 0) &
      call da_residual_profiler(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_buoyobs .and. iv%num_buoy > 0) &
      call da_residual_buoy(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_rad .and. iv%num_inst > 0) &
      call da_residual_rad(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (use_airsretobs .and. iv%num_airsr > 0) &
      call da_residual_airsr(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (trace_use) call da_trace_exit("da_calculate_residual")

end subroutine da_calculate_residual


