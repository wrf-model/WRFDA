MODULE da_gpsref

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE par_util

   ! The "stats_gpsref_type" is ONLY used locally in DA_Gpsref:

   TYPE residual_gpsref1_type
      REAL :: ref                   ! GPS Refractivity
      REAL ::   p                   ! Retrieved from GPS Refractivity
      REAL ::   t                   ! Retrieved from GPS Refractivity
      REAL ::   q                   ! Used in GPS Refra. retrieval.
   END TYPE residual_gpsref1_type

   TYPE maxmin_gpsref_stats_type
      TYPE (maxmin_type)         :: ref          ! GPS Refractivity
   END TYPE maxmin_gpsref_stats_type

   TYPE stats_gpsref_type
      TYPE (maxmin_gpsref_stats_type)  :: maximum, minimum
      TYPE (residual_gpsref1_type)     :: average, rms_err
   END TYPE stats_gpsref_type

CONTAINS

#include "DA_AO_Stats_Gpsref.inc"
#include "DA_Calculate_GradY_Gpsref.inc"
#include "DA_Get_Jo_and_GradY_Gpsref.inc"
#include "DA_Calculate_Residual_Gpsref.inc"
#include "DA_OI_Stats_Gpsref.inc"
#include "DA_Print_Stats_Gpsref.inc"
#include "DA_Transform_XToY_Gpsref.inc"
#include "DA_Transform_XToY_Gpsref_Adj.inc"
#include "da_check_max_iv_Gpsref.inc"
#include "da_get_innov_vector_Gpsref.inc"

END MODULE da_gpsref

