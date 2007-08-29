module da_wrfvar_top

   !-----------------------------------------------------------------------
   ! Purpose: 
   !-----------------------------------------------------------------------

   use module_configure, only : grid_config_rec_type,model_config_rec, &
      model_to_grid_config_rec, get_config_as_buffer,set_config_as_buffer, &
      initial_config,nl_get_debug_level
   use module_domain, only : domain,alloc_and_configure_domain, head_grid, &
      program_name
   use module_driver_constants, only : max_comms
   use module_symbols_util, only : wrfu_finalize, wrfu_initialize, &
      wrfu_cal_gregorian

   use module_radiance, only : satinfo, time_slots
#ifdef RTTOV
   use module_radiance, only : coefs, sensor_descriptor
#endif

   use module_state_description, only : num_moist, num_a_moist, num_g_moist, &
      num_scalar, num_a_scalar, num_g_scalar
   use module_tiles, only : set_tiles

#ifdef DM_PARALLEL
#ifdef RSL_LITE
   use module_dm, only : local_communicator, local_communicator_x, &
      local_communicator_y, ntasks_x, ntasks_y, data_order_xyz, mytask, &
      ntasks, data_order_xy,wrf_dm_initialize
#else
   use module_dm, only : invalid_message_value, glen,setup_xpose_rsl, &
      add_msg_24pt_real,reset_msgs_24pt,stencil_24pt,setup_halo_rsl, &
      reset_msgs_xpose, add_msg_xpose_real, define_xpose, add_msg_24pt_integer, &
      wrf_dm_initialize
#endif
#endif

   ! too many namelist options to list
   use da_control
   use da_define_structures, only : y_type, j_type, iv_type, be_type, &
      xbx_type,da_deallocate_background_errors,da_initialize_cv, &
      da_zero_vp_type,da_allocate_y,da_deallocate_observations, &
      da_deallocate_y
   use da_minimisation, only : da_get_innov_vector,da_minimise_cg, &
      da_write_diagnostics
   use da_radiance1, only : da_write_oa_rad_ascii
   use da_obs_io, only : da_write_filtered_obs
   use da_par_util, only : da_system,da_copy_tile_dims,da_copy_dims
   use da_physics, only : da_uvprho_to_w_lin
   use da_radiance1, only : num_tovs_before, tovs_recv_pe,tovs_copy_count, &
      tovs_send_pe,tovs_send_count,tovs_recv_start, num_tovs_after, &
      tovs_send_start
   use da_reporting, only : message, da_warning, da_error, da_message
   use da_setup_structures, only : da_setup_obs_structures, &
      da_setup_background_errors,da_setup_flow_predictors
   use da_test, only : da_check
   use da_tools_serial, only : da_get_unit, da_free_unit
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace
   use da_transfer_model, only : da_transfer_xatoanalysis,da_setup_firstguess
   use da_vtox_transforms, only : da_transform_vtox
   use da_wrfvar_io, only : da_med_initialdata_input, da_med_initialdata_output

#ifdef CRTM
   use module_radiance, only : crtm_destroy
   use da_crtm, only : channelinfo, sensor_descriptor
   use da_control, only : rtm_option, use_rad
#endif

   use da_wrf_interfaces

   implicit none

   integer :: loop, levels_to_process

   type (domain) , pointer :: keep_grid, grid_ptr, null_domain
   type (grid_config_rec_type), save :: config_flags
   integer                 :: number_at_same_level
   integer                 :: time_step_begin_restart

   integer :: domain_id , fid , oid , idum1 , idum2

#ifdef DM_PARALLEL
   integer                 :: nbytes
   integer, parameter      :: configbuflen = 4* CONFIG_BUF_LEN
   integer                 :: configbuf( configbuflen )
#endif

   character (len=80)      :: rstname

contains

#include "da_wrfvar_init1.inc"
#include "da_wrfvar_init2.inc"
#include "da_wrfvar_run.inc"
#include "da_wrfvar_interface.inc"
#include "da_wrfvar_finalize.inc"
#include "da_solve.inc"

end module da_wrfvar_top
