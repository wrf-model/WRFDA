MODULE DA_Synop

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

! The "stats_synop_type" is ONLY used locally in DA_Synop:

   TYPE residual_synop1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_synop1_type

   TYPE maxmin_synop_stats_type
        TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_synop_stats_type

   TYPE stats_synop_type
        TYPE (maxmin_synop_stats_type)  :: maximum, minimum
        TYPE (residual_synop1_type)     :: average, rms_err
   END TYPE stats_synop_type

CONTAINS

#include "DA_AO_Stats_Synop.inc"
#include "DA_Calculate_Jo_and_GradY_Synop.inc"
#include "DA_Calculate_Residual_Synop.inc"
#include "DA_OI_Stats_Synop.inc"
#include "DA_Print_Stats_Synop.inc"
#include "DA_Transform_XToY_Synop.inc"
#include "DA_Transform_XToY_Synop_Adj.inc"
#include "da_get_innov_vector_synop.inc"
#include "da_check_max_iv_synop.inc"
#include "DA_Calculate_GradY_Synop.inc"

END MODULE DA_Synop

