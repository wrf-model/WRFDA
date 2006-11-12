module da_gpsref

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_gpsref_type" is ONLY used locally in da_gpsref:

   type residual_gpsref1_type
      real :: ref                   ! GPS Refractivity
      real ::   p                   ! Retrieved from GPS Refractivity
      real ::   t                   ! Retrieved from GPS Refractivity
      real ::   q                   ! Used in GPS Refra. retrieval.
   end type residual_gpsref1_type

   type maxmin_gpsref_stats_type
      type (maxmin_type)         :: ref          ! GPS Refractivity
   end type maxmin_gpsref_stats_type

   type stats_gpsref_type
      type (maxmin_gpsref_stats_type)  :: maximum, minimum
      type (residual_gpsref1_type)     :: average, rms_err
   end type stats_gpsref_type

contains

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

