subroutine da_calculate_residual(iv, y, re)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals
   !-----------------------------------------------------------------------

   implicit none
      
   type (ob_type), intent(in)        :: iv     ! Innovation vector (O-B).
   type (y_type), intent(in)         :: y      ! y = H (xa)
   type (y_type), intent(inout)      :: re     ! Residual (O-A).

   integer    :: np_available, np_obs_used, np_missing  , np_bad_data 

   if (trace_use) call da_trace_entry("da_calculate_residual")
      
   np_available = 0
   np_obs_used  = 0
   np_missing   = 0
   np_bad_data  = 0

   !-------------------------------------------------------------------------
   ! [1.0] (O-A) = (O-B) - H x~:
   !-------------------------------------------------------------------------

   ! for radiosonde:

   call da_residual_sound(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   call da_residual_sonde_sfc(iv, y, re, np_missing, np_bad_data, &
                                         np_obs_used, np_available)
!whl
   ! for mtgirs
   call da_residual_mtgirs(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)
   ! for surface

   call da_residual_synop(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for Geo. Satellite AMVs :

   call da_residual_geoamv(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for Polar Satellite AMVs :

   call da_residual_polaramv(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for airep:

   call da_residual_airep(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for metar

   call da_residual_metar(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for ships

   call da_residual_ships(iv, y, re, np_missing, np_bad_data, & 
                                     np_obs_used, np_available)

   ! GPSPW:

   call da_residual_gpspw(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available) 

   ! GPSRF:

   call da_residual_gpsref(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available) 

   ! for SSMI

   call da_residual_ssmi(iv, y, re, np_missing, np_bad_data, &
                              np_obs_used, np_available)
   ! for SSMT1

   call da_residual_ssmt1(iv, y, re, np_missing, np_bad_data, &
                               np_obs_used, np_available)

   ! for SSMT2

   call da_residual_ssmt2(iv, y, re, np_missing, np_bad_data, &
                               np_obs_used, np_available)

   ! for pilot

   call da_residual_pilot(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for TC bogus

   call da_residual_bogus(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for satem

   call da_residual_satem(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! Pseudo-obs

   call da_residual_pseudo(iv, y, re, &
                                      np_missing, np_bad_data, &
                                      np_obs_used, np_available)
   ! Quikscat scatterometer:

   call da_residual_qscat(iv, y, re, &
                                     np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for radar

   call da_residual_radar(iv, y, re, &
                                     np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for profiler

   call da_residual_profiler(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for buoy   

   call da_residual_buoy(iv, y, re, np_missing, np_bad_data, & 
                                     np_obs_used, np_available)

   ! for radiance
   call da_residual_rad(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   ! for AIRS retrievals

   call da_residual_airsr(iv, y, re, np_missing, np_bad_data, &
                                     np_obs_used, np_available)

   if (trace_use) call da_trace_exit("da_calculate_residual")


end subroutine da_calculate_residual


