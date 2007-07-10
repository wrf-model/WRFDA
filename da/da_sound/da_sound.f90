module da_sound

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing, max_error_uv, max_error_t, rootproc, &
      num_sound_tot, max_error_p,max_error_q, sfc_assi_options, &
      max_stheight_diff,test_dm_exact, trace_use, anal_type_verify, &
      ims,ime,jms,jme,kms,kme,kts,kte
   use da_define_structures, only : maxmin_type, ob_type, y_type, jo_type, &
      bad_data_type, x_type, sound_type, number_type, bad_data_type, &
      residual_sound_type,synop_type
   use module_domain, only : xpose_type, xb_type, domain
   use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj,da_interp_obs_lin_2d,da_interp_obs_lin_2d_adj, &
      da_interp_lin_2d, da_interp_lin_2d_adj
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_obs_sfc_correction
   use da_tools1, only : da_get_unit, da_free_unit
   use da_par_util, only : da_proc_stats_combine, &
      da_deallocate_global_sound, da_to_global_sound, da_to_global_sonde_sfc, &
      da_deallocate_global_sonde_sfc
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_sfc_pre, da_transform_xtopsfc, &
      da_transform_xtopsfc_adj
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_sound_type" is ONLY used locally in da_sound:

   type residual_sound1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_sound1_type

   type maxmin_sound_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_sound_stats_type

   type stats_sound_type
      type (maxmin_sound_stats_type)  :: maximum, minimum
      type (residual_sound1_type)     :: average, rms_err
   end type stats_sound_type

   ! The "stats_sonde_sfc_type" is ONLY used locally in da_sonde_sfc:

   type residual_sonde_sfc1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_sonde_sfc1_type

   type maxmin_sonde_sfc_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_sonde_sfc_stats_type

   type stats_sonde_sfc_type
      type (maxmin_sonde_sfc_stats_type)  :: maximum, minimum
      type (residual_sonde_sfc1_type)     :: average, rms_err
   end type stats_sonde_sfc_type

contains

#include "da_ao_stats_sound.inc"
#include "da_jo_and_grady_sound.inc"
#include "da_jo_sound_uvtq.inc"
#include "da_residual_sound.inc"
#include "da_oi_stats_sound.inc"
#include "da_print_stats_sound.inc"
#include "da_transform_xtoy_sound.inc"
#include "da_transform_xtoy_sound_adj.inc"
#include "da_check_max_iv_sound.inc"
#include "da_get_innov_vector_sound.inc"
#include "da_obs_diagnostics.inc"
#include "da_calculate_grady_sound.inc"

#include "da_ao_stats_sonde_sfc.inc"
#include "da_jo_and_grady_sonde_sfc.inc"
#include "da_jo_sonde_sfc_uvtq.inc"
#include "da_residual_sonde_sfc.inc"
#include "da_oi_stats_sonde_sfc.inc"
#include "da_print_stats_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc_adj.inc"
#include "da_get_innov_vector_sonde_sfc.inc"
#include "da_check_max_iv_sonde_sfc.inc"
#include "da_calculate_grady_sonde_sfc.inc"

end module da_sound

