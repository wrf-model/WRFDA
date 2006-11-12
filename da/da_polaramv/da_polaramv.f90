module da_polaramv

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

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
#include "da_get_jo_and_grady_polaramv.inc"
#include "da_calculate_residual_polaramv.inc"
#include "da_oi_stats_polaramv.inc"
#include "da_print_stats_polaramv.inc"
#include "da_transform_xtoy_polaramv.inc"
#include "da_transform_xtoy_polaramv_adj.inc"
#include "da_check_max_iv_polaramv.inc"
#include "da_get_innov_vector_polaramv.inc"
#include "da_calculate_grady_polaramv.inc"

end module da_polaramv     

