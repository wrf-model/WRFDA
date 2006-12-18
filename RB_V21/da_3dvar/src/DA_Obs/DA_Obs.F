MODULE DA_Obs

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Airep
   USE DA_gpspw
   USE DA_gpsref
   USE DA_Metar
   USE DA_Pilot
   USE DA_Radar
   USE DA_ssmi
   USE DA_Satem
   USE DA_GeoAMV
   USE DA_PolarAMV
   USE DA_Ships
   USE DA_Synop
   USE DA_Sound
   USE DA_Bogus
   USE da_pseudo
   USE da_qscat
   USE DA_Profiler
   USE DA_Buoy 
   USE par_util
   USE DA_Tools

   IMPLICIT NONE

CONTAINS

#include "DA_Obs_Proc_Station.inc"
#include "DA_Read_Obs.inc"
#include "DA_Scan_Obs.inc"
#include "DA_Read_Radar.inc"
#include "DA_Scan_Radar.inc"
#include "DA_Transform_XToY.inc"
#include "DA_Transform_XToY_Adj.inc"
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
#include "DA_Count_Filtered_Obs.inc"

END MODULE DA_Obs
