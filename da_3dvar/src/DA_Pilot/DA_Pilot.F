MODULE DA_Pilot

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_pilot_type" is ONLY used locally in DA_Pilot:

   TYPE residual_pilot1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_pilot1_type

   TYPE maxmin_pilot_stats_type
        TYPE (maxmin_type)         :: u, v
   END TYPE maxmin_pilot_stats_type

   TYPE stats_pilot_type
        TYPE (maxmin_pilot_stats_type)  :: maximum, minimum
        TYPE (residual_pilot1_type)     :: average, rms_err
   END TYPE stats_pilot_type

CONTAINS

#include "DA_AO_Stats_Pilot.inc"
#include "DA_Calculate_Jo_and_GradY_Pilot.inc"
#include "DA_Calculate_Residual_Pilot.inc"
#include "DA_OI_Stats_Pilot.inc"
#include "DA_Print_Stats_Pilot.inc"
#include "DA_Transform_XToY_Pilot.inc"
#include "DA_Transform_XToY_Pilot_Adj.inc"
#include "da_check_max_iv_pilot.inc"
#include "da_get_innov_vector_pilot.inc"
#include "DA_Calculate_GradY_Pilot.inc"

END MODULE DA_Pilot

