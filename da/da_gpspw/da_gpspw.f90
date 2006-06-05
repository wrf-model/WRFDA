MODULE da_gpspw

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_physics
   USE da_tools
   USE da_par_util

   ! The "stats_gpspw_type" is ONLY used locally in DA_Gpspw:

   TYPE residual_gpspw1_type
      REAL          :: tpw                      ! Precipitable water
   END TYPE residual_gpspw1_type

   TYPE maxmin_gpspw_stats_type
      TYPE (maxmin_type)         :: tpw
   END TYPE maxmin_gpspw_stats_type

   TYPE stats_gpspw_type
      TYPE (maxmin_gpspw_stats_type)  :: maximum, minimum
      TYPE (residual_gpspw1_type)     :: average, rms_err
   END TYPE stats_gpspw_type

CONTAINS

#include "da_ao_stats_gpspw.inc"
#include "da_calculate_jo_and_grady_gpspw.inc"
#include "da_calculate_residual_gpspw.inc"
#include "da_oi_stats_gpspw.inc"
#include "da_print_stats_gpspw.inc"
#include "da_transform_xtoy_gpspw.inc"
#include "da_transform_xtoy_gpspw_adj.inc"
#include "da_check_max_iv_gpspw.inc"
#include "da_get_innov_vector_gpspw.inc"
#include "da_calculate_grady_gpspw.inc"


end module da_gpspw

