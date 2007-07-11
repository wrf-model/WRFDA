subroutine da_write_diagnostics(grid, ob, iv, re, y, j)

   !---------------------------------------------------------------------------
   ! Purpose: Output data assimilation diagnostics
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)    :: grid
   type (y_type), intent(in)    :: ob      ! Observation structure.
   type (ob_type), intent(inout):: iv      ! innovation vector.
   type (y_type), intent(inout) :: re      ! residual vector.
   type (y_type), intent(in)    :: y       ! y = H(x_inc) structure.
   type (j_type), intent(inout) :: j       ! Cost function.

   if (trace_use) call da_trace_entry("da_write_diagnostics")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate innovation vector (O-B) statistics:
   !---------------------------------------------------------------------------

   ! [1.1] Sondes:

   call da_oi_stats_sound (stats_unit, iv)
   call da_oi_stats_sonde_sfc (stats_unit, iv)

   ! [1.2] Surface:

   call da_oi_stats_synop (stats_unit, iv)

   ! [1.3.1] Geostationary Satellite Cloud track winds:

   call da_oi_stats_geoamv (stats_unit, iv)

   ! [1.3.2] Polar Satellite Cloud track winds:

   call da_oi_stats_polaramv (stats_unit, iv)

   ! [1.4] Aireps:

   call da_oi_stats_airep (stats_unit, iv)

   ! [1.5] Pilot:

   call da_oi_stats_pilot (stats_unit, iv)

   ! [1.6] Metar:

   call da_oi_stats_metar (stats_unit, iv)

   ! [1.7] Ships:

   call da_oi_stats_ships (stats_unit, iv)

   ! [1.8] GPSPW:

   call da_oi_stats_gpspw (stats_unit, iv)

   ! [1.9] GPSPW:

   call da_oi_stats_gpsref (stats_unit, iv)

   ! [1.10] SSMI:

   call da_oi_stats_ssmi (stats_unit, iv)

   ! [1.11] SATEMs:

   call  da_oi_stats_satem (stats_unit, iv)

   ! [1.12] Pseudo obs:

   call da_oi_stats_pseudo (stats_unit, iv)

   ! [1.13] SSM/T1:

   call da_oi_stats_ssmt1 (stats_unit, iv)

   ! [1.14] SSM/T2:

   call da_oi_stats_ssmt2 (stats_unit, iv)

   ! [1.15] Quikscat:

   call da_oi_stats_qscat (stats_unit, iv)

   ! [1.16] Profiler:

   call da_oi_stats_profiler (stats_unit, iv)

   ! [1.17] Buoy:  

   call da_oi_stats_buoy (stats_unit, iv)

   ! [1.18] radar: 

   call da_oi_stats_radar (stats_unit, iv)

   ! [1.19] TC bogus:

   call da_oi_stats_bogus (stats_unit, iv)

   ! [1.20] Radiance:

   call da_oi_stats_rad (stats_unit, iv)

   ! [1.21] AIRS retrievals:

   call da_oi_stats_airsr (stats_unit, iv)

   !---- ----------------------------------------------------------------------
   ! [2.0] Calculate residual vector (O-A) statistics:
   !---------------------------------------------------------------------------

   ! [2.1] Sondes:

   call da_ao_stats_sound (stats_unit, iv, re)
   call da_ao_stats_sonde_sfc (stats_unit, iv, re)

   ! [2.2] Surface:

   call da_ao_stats_synop (stats_unit, iv, re)

   ! [2.3.1] Geostationary Satellite Cloud track winds:

   call da_ao_stats_geoamv (stats_unit, iv, re)

   ! [2.3.2] Polar Satellite Cloud track winds:

   call da_ao_stats_polaramv (stats_unit, iv, re)

   ! [2.4] Aireps:

   call da_ao_stats_airep (stats_unit, iv, re)

   ! [2.5] Pilot:

   call da_ao_stats_pilot (stats_unit, iv, re)

   ! [2.6] Metar:

   call da_ao_stats_metar (stats_unit, iv, re)

   ! [2.7] Ships:

   call da_ao_stats_ships (stats_unit, iv, re)

   ! [2.8] GPSPW:

   call da_ao_stats_gpspw (stats_unit, iv, re)

   ! [2.9] GPSRF:

   call da_ao_stats_gpsref (stats_unit, iv, re)

   ! [2.10] SSMI:

   call da_ao_stats_ssmi (stats_unit, iv, re)

   ! [2.11] SATEMs:

   call da_ao_stats_satem (stats_unit, iv, re)

   ! [2.12] Pseudo obs:

   call da_ao_stats_pseudo (stats_unit, iv, re)

   ! [2.13] SSM/T1:

   call da_ao_stats_ssmt1 (stats_unit, iv, re)

   ! [2.14] SSM/T2:

   call da_ao_stats_ssmt2 (stats_unit, iv, re)

   ! [2.15] Quikscat:

   call da_ao_stats_qscat (stats_unit, iv, re)

   ! [2.16] Profiler

   call da_ao_stats_profiler (stats_unit, iv, re)

   ! [2.17] Buoy: 

   call da_ao_stats_buoy (stats_unit, iv, re)

   !  [2.18] radar:

   call da_ao_stats_radar (stats_unit, iv, re)

   ! [2.19] TC bogus:

   call da_ao_stats_bogus (stats_unit, iv, re)

   ! [2.20] Radiance:

   call da_ao_stats_rad (stats_unit, iv, re)

   ! [2.21] AIRS Retrieval:

   call da_ao_stats_airsr (stats_unit, iv, re)

   !---------------------------------------------------------------------------
   ! [3.0] Calculate analysis increment (A-B) statistics:
   !---------------------------------------------------------------------------

   call da_analysis_stats (grid, stats_unit)

   !---------------------------------------------------------------------------
   ! [4.0] Write VAR diagnostic :
   !------------------------------------------------------------------------------

   call da_get_var_diagnostics (iv, j)

   !---- -------------------------------------------------------------------------
   !  [5.0] Write observation data (O, O-B, O-A, y=hx'):
   !------------------------------------------------------------------------------
   call da_write_obs(ob, iv, re)

   call da_final_write_obs(iv)

   call da_write_y(iv, y)

   call da_final_write_y(iv)

   if (trace_use) call da_trace_exit("da_write_diagnostics")

end subroutine da_write_diagnostics


