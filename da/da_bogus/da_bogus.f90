module da_bogus

   use da_constants
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics
   use da_par_util

   ! The "stats_bogus_type" is ONLY used locally in da_bogus:

   TYPE residual_bogus1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: q                        ! specific humidity
      REAL          :: slp                      ! sea level pressure (hPa)
   END TYPE residual_bogus1_type

   TYPE maxmin_bogus_stats_type
      TYPE (maxmin_type)         :: u, v, t, q, slp 
   END TYPE maxmin_bogus_stats_type

   TYPE stats_bogus_type
      TYPE (maxmin_bogus_stats_type)  :: maximum, minimum
      TYPE (residual_bogus1_type)     :: average, rms_err
   END TYPE stats_bogus_type

CONTAINS

#include "da_ao_stats_bogus.inc"
#include "da_calculate_jo_and_grady_bogus.inc"
#include "da_calculate_residual_bogus.inc"
#include "da_oi_stats_bogus.inc"
#include "da_print_stats_bogus.inc"
#include "da_transform_xtoy_bogus.inc"
#include "da_transform_xtoy_bogus_adj.inc"
#include "da_check_max_iv_bogus.inc"
#include "da_get_innov_vector_bogus.inc"
#include "da_calculate_grady_bogus.inc"

end module da_bogus

