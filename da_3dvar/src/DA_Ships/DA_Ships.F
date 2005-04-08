MODULE DA_Ships

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

! The "stats_ships_type" is ONLY used locally in DA_Ships:

   TYPE residual_ships1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_ships1_type

   TYPE maxmin_ships_stats_type
        TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_ships_stats_type

   TYPE stats_ships_type
        TYPE (maxmin_ships_stats_type)  :: maximum, minimum
        TYPE (residual_ships1_type)     :: average, rms_err
   END TYPE stats_ships_type

CONTAINS

#include "DA_AO_Stats_Ships.inc"
#include "DA_Calculate_Jo_and_GradY_Ships.inc"
#include "DA_Calculate_Residual_Ships.inc"
#include "DA_OI_Stats_Ships.inc"
#include "DA_Print_Stats_Ships.inc"
#include "DA_Transform_XToY_Ships.inc"
#include "DA_Transform_XToY_Ships_Adj.inc"
#include "da_check_max_iv_ships.inc"
#include "da_get_innov_vector_ships.inc"
#include "DA_Calculate_GradY_Ships.inc"


END MODULE DA_Ships

