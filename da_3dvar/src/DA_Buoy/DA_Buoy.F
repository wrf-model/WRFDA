MODULE DA_Buoy 

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

! The "stats_buoy_type" is ONLY used locally in DA_Buoy:

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

#include "DA_AO_Stats_Buoy.inc"
#include "DA_Calculate_Jo_and_GradY_Buoy.inc"
#include "DA_Calculate_Residual_Buoy.inc"
#include "DA_OI_Stats_Buoy.inc"
#include "DA_Print_Stats_Buoy.inc"
#include "DA_Transform_XToY_Buoy.inc"
#include "DA_Transform_XToY_Buoy_Adj.inc"
#include "da_check_max_iv_buoy.inc"
#include "da_get_innov_vector_buoy.inc"
#include "DA_Calculate_GradY_Buoy.inc"


END MODULE DA_Buoy 

