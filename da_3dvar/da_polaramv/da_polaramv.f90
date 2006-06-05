MODULE DA_PolarAMV

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_polaramv_type" is ONLY used locally in DA_PolarAMV:

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

#include "DA_AO_Stats_PolarAMV.inc"
#include "DA_Get_Jo_and_GradY_PolarAMV.inc"
#include "DA_Calculate_Residual_PolarAMV.inc"
#include "DA_OI_Stats_PolarAMV.inc"
#include "DA_Print_Stats_PolarAMV.inc"
#include "DA_Transform_XToY_PolarAMV.inc"
#include "DA_Transform_XToY_PolarAMV_Adj.inc"
#include "da_check_max_iv_polaramv.inc"
#include "da_get_innov_vector_polaramv.inc"
#include "DA_Calculate_GradY_PolarAMV.inc"

END MODULE DA_PolarAMV     

