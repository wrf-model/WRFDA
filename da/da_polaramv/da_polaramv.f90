module da_polaramv

   use module_domain, only : xpose_type, xb_type, domain
   
   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, trace_use, &
      missing, max_error_uv, max_error_t, rootproc, kms,kme,kts,kte, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, num_polaramv_tot, anal_type_verify
   use da_define_structures, only : maxmin_type, ob_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      polaramv_type
   use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_polaramv_type" is ONLY used locally in da_polaramv:

   type residual_polaramv1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_polaramv1_type

   type maxmin_polaramv_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_polaramv_stats_type

   type stats_polaramv_type
      type (maxmin_polaramv_stats_type)  :: maximum, minimum
      type (residual_polaramv1_type)     :: average, rms_err
   end type stats_polaramv_type


contains

#include "da_ao_stats_polaramv.inc"
#include "da_jo_and_grady_polaramv.inc"
#include "da_residual_polaramv.inc"
#include "da_oi_stats_polaramv.inc"
#include "da_print_stats_polaramv.inc"
#include "da_transform_xtoy_polaramv.inc"
#include "da_transform_xtoy_polaramv_adj.inc"
#include "da_check_max_iv_polaramv.inc"
#include "da_get_innov_vector_polaramv.inc"
#include "da_calculate_grady_polaramv.inc"

end module da_polaramv     

