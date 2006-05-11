MODULE da_gpspw

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_physics
   USE da_tools
   USE par_util

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

#include "DA_AO_Stats_Gpspw.inc"
#include "DA_Calculate_Jo_and_GradY_Gpspw.inc"
#include "DA_Calculate_Residual_Gpspw.inc"
#include "DA_OI_Stats_Gpspw.inc"
#include "DA_Print_Stats_Gpspw.inc"
#include "DA_Transform_XToY_Gpspw.inc"
#include "DA_Transform_XToY_Gpspw_Adj.inc"
#include "da_check_max_iv_gpspw.inc"
#include "da_get_innov_vector_gpspw.inc"
#include "DA_Calculate_GradY_Gpspw.inc"


END MODULE da_gpspw

