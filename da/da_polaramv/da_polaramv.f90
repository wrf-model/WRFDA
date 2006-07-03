module da_polaramv

   use da_constants
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_polaramv_type" is ONLY used locally in da_polaramv:

   TYPE residual_polaramv1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_polaramv1_type

   TYPE maxmin_polaramv_stats_type
      TYPE (maxmin_type)         :: u, v, t, q
   END TYPE maxmin_polaramv_stats_type

   TYPE stats_polaramv_type
      TYPE (maxmin_polaramv_stats_type)  :: maximum, minimum
      TYPE (residual_polaramv1_type)     :: average, rms_err
   END TYPE stats_polaramv_type


CONTAINS

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

