module da_obs_io

   use module_domain, only : xpose_type, xb_type

   use da_control, only : missing_r, fmt_each, fmt_info, trace_use, &
      fmt_srfc, filtered_obs_unit, num_procs,missing, ierr,comm, rand_unit, &
      obs_qc_pointer, rootproc, omb_unit,omb_add_noise,use_airepobs, &
      use_airepobs,use_bogusobs,use_gpspwobs,use_gpsrefobs,use_geoamvobs, &
      use_metarobs,use_profilerobs,use_pilotobs,use_buoyobs,use_shipsobs, &
      use_synopobs,use_soundobs,use_qscatobs,testing_wrfvar, report_start, &
      report_end, global, print_detail_obs, stdout, t_kelvin, stderr, &
      max_ob_levels, missing_data, max_bogus_input, myproc,convert_uv2fd, &
      fails_error_max,standard_atmosphere,zero_t_td,print_detail_f_obs, &
      max_radar, print_detail_radar,use_satemobs,use_polaramvobs,use_ssmt1obs, &
      use_ssmt2obs, use_airsretobs,convert_fd2uv,anal_type_qcobs,gravity, &
      filename_len
   use da_define_structures, only : ob_type, multi_level_type, &
      radar_multi_level_type, y_type, field_type, each_level_type, &
      radar_each_level_type
   use da_grid_definitions, only : da_earth_2_model_wind,da_ffdduv
   use da_obs, only : da_count_filtered_obs,da_check_missing,da_obs_proc_station
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_tp_to_qs
   use da_reporting, only : da_warning, message, da_error, da_message
   use da_tools, only : da_ll_to_xy
   use da_tools1, only : da_free_unit, da_get_unit
   use da_tracing, only : da_trace_entry, da_trace_exit

   implicit none

contains

#include "da_read_obs.inc"
#include "da_scan_obs.inc"
#include "da_read_radar.inc"
#include "da_scan_radar.inc"
#include "da_read_errfac.inc"
#include "da_use_obs_errfac.inc"
#include "da_write_obs.inc"
#include "da_write_filtered_obs.inc"
#include "da_write_y.inc"
#include "da_read_bufr_obs.inc"
#include "da_scan_bufr_obs.inc"
#include "da_final_write_obs.inc"
#include "da_final_write_y.inc"
#include "da_read_y_unit.inc"
#include "da_read_rand_unit.inc"
#include "da_read_omb_tmp.inc"
#include "da_write_noise_to_ob.inc"
#include "da_final_write_filtered_obs.inc"

end module da_obs_io
