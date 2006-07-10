MODULE da_geoamv

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_par_util

   ! The "stats_geoamv_type" is ONLY used locally in da_geoamv:

   TYPE residual_geoamv1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_geoamv1_type

   TYPE maxmin_geoamv_stats_type
      TYPE (maxmin_type)         :: u, v, t, q
   END TYPE maxmin_geoamv_stats_type

   TYPE stats_geoamv_type
      TYPE (maxmin_geoamv_stats_type)  :: maximum, minimum
      TYPE (residual_geoamv1_type)     :: average, rms_err
   END TYPE stats_geoamv_type

CONTAINS

#include "da_ao_stats_geoamv.inc"
#include "da_get_jo_and_grady_geoamv.inc"
#include "da_calculate_residual_geoamv.inc"
#include "da_oi_stats_geoamv.inc"
#include "da_print_stats_geoamv.inc"
#include "da_transform_xtoy_geoamv.inc"
#include "da_transform_xtoy_geoamv_adj.inc"
#include "da_check_max_iv_geoamv.inc"
#include "da_get_innov_vector_geoamv.inc"
#include "da_calculate_grady_geoamv.inc"

end module da_geoamv     

