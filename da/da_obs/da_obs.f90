module da_obs

   use da_control
   use da_define_structures
   use da_airep
   use da_gpspw
   use da_gpsref
   use da_metar
   use da_pilot
   use da_radar
   use da_ssmi
   use da_satem
   use da_geoamv
   use da_polaramv
   use da_ships
   use da_synop
   use da_sound
   use da_bogus
   use da_pseudo
   use da_qscat
   use da_profiler
   use da_buoy 
   use da_par_util
   use da_tools
   use da_tracing
   use da_radiance
   use da_airsr   
   use da_reporting

   IMPLICIT NONE

CONTAINS

#include "da_obs_proc_station.inc"
#include "da_read_obs.inc"
#include "da_scan_obs.inc"
#include "da_read_radar.inc"
#include "da_scan_radar.inc"
#include "da_transform_xtoy.inc"
#include "da_transform_xtoy_adj.inc"
#include "da_add_noise_to_ob.inc"
#include "da_check_missing.inc"
#include "da_fill_obs_structures.inc"
#include "da_random_omb_all.inc"
#include "da_read_errfac.inc"
#include "da_setup_pseudo_obs.inc"
#include "da_store_obs_grid_info.inc"
#include "da_use_obs_errfac.inc"
#include "da_write_obs.inc"
#include "da_write_filtered_obs.inc"
#include "da_write_y.inc"
#include "da_read_bufr_obs.inc"
#include "da_scan_bufr_obs.inc"
#include "da_count_filtered_obs.inc"
#include "da_final_write_obs.inc"
#include "da_final_write_y.inc"
#include "da_read_y_unit.inc"
#include "da_read_rand_unit.inc"
#include "da_read_omb_tmp.inc"
#include "da_write_noise_to_ob.inc"
#include "da_final_write_filtered_obs.inc"

end module da_obs
