MODULE DA_GeoAMV

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_geoamv_type" is ONLY used locally in DA_GeoAMV:

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

#include "DA_AO_Stats_GeoAMV.inc"
#include "DA_Get_Jo_and_GradY_GeoAMV.inc"
#include "DA_Calculate_Residual_GeoAMV.inc"
#include "DA_OI_Stats_GeoAMV.inc"
#include "DA_Print_Stats_GeoAMV.inc"
#include "DA_Transform_XToY_GeoAMV.inc"
#include "DA_Transform_XToY_GeoAMV_Adj.inc"
#include "da_check_max_iv_geoamv.inc"
#include "da_get_innov_vector_geoamv.inc"
#include "DA_Calculate_GradY_GeoAMV.inc"

END MODULE DA_GeoAMV     

