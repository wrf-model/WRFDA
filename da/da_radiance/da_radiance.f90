module da_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use module_domain, only : xpose_type, xb_type
   use module_radiance, only : coefs,satinfo, coefs_scatt_instname, &
      coefs_scatt,profile_type,radiance_type,transmission_type,time_slots, &
      rttov_coef,i_kind,r_kind, r_double, CRTM_ChannelInfo_type, &
      Sensor_Descriptor,crtm_platform_name,crtm_sensor_name, platform_name, &
      inst_name, crtm_init, one, zero, three,deg2rad,rad2deg, &
      errorstatus_success, n_scatt_coef,q2ppmv,gas_id_watervapour, &
      init_constants_derived, gsi_emiss

#ifdef DM_PARALLEL
   use mpi, only : mpi_integer, mpi_status_size
#endif

   use da_control, only : max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, &
      missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, rtminit_platform,rtminit_satid, &
      rtminit_nsensor,rtminit_sensor,filename_len,read_biascoef,analysis_date, &
      time_window_max,time_window_min,print_detail_obs,use_hsbobs,use_msuobs, &
      use_amsubobs,use_eos_amsuaobs,use_amsuaobs,use_hirs2obs,rtm_option, &
      rtm_option_rttov,rtm_option_crtm,use_airsobs,use_kma1dvar,use_hirs3obs, &
      use_filtered_rad,print_detail_radiance,stderr, mw_emis_sea, &
      rtminit_print, rttov_scatt,comm,ierr,biasprep, qc_rad, num_procs, &
      tovs_min_transfer,use_error_factor_rad,num_fgat_time,stdout,trace_use, &
      qc_good, qc_bad,myproc,biascorr
   use da_define_structures, only : maxmin_type, ob_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      airsr_type,info_type, model_loc_type
   use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj,da_interp_obs_lin_2d,da_interp_obs_lin_2d_adj, &
      da_interp_lin_2d, da_interp_lin_2d_adj,da_interp_lin_2d_new, &
      da_interp_lin_3d_new, da_interp_lin_3d_adj_new, &
      da_to_zk_new, da_interp_lin_2d_adj_new
   use da_tools1, only : da_get_unit, da_free_unit, da_oi
   use da_par_util1, only : da_proc_sum_int,da_proc_sum_ints
#ifdef DM_PARALLEL
   use da_par_util, only :  da_proc_stats_combine, true_mpi_real
#else
   use da_par_util, only :  da_proc_stats_combine
#endif
   use da_physics, only : da_sfc_pre, da_transform_xtopsfc, &
      da_transform_xtopsfc_adj,da_tpq_to_slp_lin,da_tpq_to_slp_adj
   use da_radiance1, only : num_tovs_before,num_tovs_after,tovs_copy_count, &
      tovs_send_pe, tovs_recv_pe, tovs_send_start, tovs_send_count, &
      tovs_recv_start,con_vars_type,aux_vars_type, datalink_type,da_qc_amsub, &
      da_qc_amsua,da_biascorr_rad, da_detsurtyp,da_biasprep
   use da_reporting, only : da_message, da_warning, message, da_error
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_obs_sfc_correction, &
      da_ll_to_xy, da_togrid_new
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace, &
      da_trace_int_sort
   use da_wrf_interfaces, only : wrf_dm_bcast_integer

   implicit none
   
contains

#include "da_qc_rad.inc"
#include "da_get_innov_vector_rad.inc"
#include "da_transform_xtoy_rad.inc"
#include "da_transform_xtoy_rad_adj.inc"
#include "da_calculate_grady_rad.inc"

#include "da_read_filtered_rad.inc"
#include "da_write_filtered_rad.inc"
#include "da_get_julian_time.inc"
#include "da_get_time_slots.inc"

#include "da_rttov_init.inc"
#include "da_rttov_direct.inc"
#include "da_rttov_tl.inc"
#include "da_rttov_ad.inc"
#include "da_read_bufrtovs.inc"
#include "da_read_bufrairs.inc"
#include "da_read_kma1dvar.inc"
#include "da_sort_rad.inc"
#include "da_setup_bufrtovs_structures.inc"
#include "da_status_rad.inc"

! This CRTM file has to be in da_radiance to avoid a circular dependancy

#include "da_crtm_init.inc"
#include "da_crtm_sensor_descriptor.inc"

end module da_radiance

