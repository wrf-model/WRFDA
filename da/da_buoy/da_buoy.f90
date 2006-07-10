module da_buoy 

   use da_constants
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics
   use da_par_util

   ! The "stats_buoy_type" is ONLY used locally in da_buoy:

   TYPE residual_buoy1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_buoy1_type

   TYPE maxmin_buoy_stats_type
      TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_buoy_stats_type

   TYPE stats_buoy_type
      TYPE (maxmin_buoy_stats_type)  :: maximum, minimum
      TYPE (residual_buoy1_type)     :: average, rms_err
   END TYPE stats_buoy_type

CONTAINS

#include "da_ao_stats_buoy.inc"
#include "da_calculate_jo_and_grady_buoy.inc"
#include "da_calculate_residual_buoy.inc"
#include "da_oi_stats_buoy.inc"
#include "da_print_stats_buoy.inc"
#include "da_transform_xtoy_buoy.inc"
#include "da_transform_xtoy_buoy_adj.inc"
#include "da_check_max_iv_buoy.inc"
#include "da_get_innov_vector_buoy.inc"
#include "da_calculate_grady_buoy.inc"


end module da_buoy 

