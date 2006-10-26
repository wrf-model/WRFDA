module da_gpsref

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_gpsref_type" is ONLY used locally in da_gpsref:

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

#include "da_ao_stats_gpsref.inc"
#include "da_calculate_grady_gpsref.inc"
#include "da_get_jo_and_grady_gpsref.inc"
#include "da_calculate_residual_gpsref.inc"
#include "da_oi_stats_gpsref.inc"
#include "da_print_stats_gpsref.inc"
#include "da_transform_xtoy_gpsref.inc"
#include "da_transform_xtoy_gpsref_adj.inc"
#include "da_check_max_iv_gpsref.inc"
#include "da_get_innov_vector_gpsref.inc"

end module da_gpsref

