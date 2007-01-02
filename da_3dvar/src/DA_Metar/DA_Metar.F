MODULE DA_Metar

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

! The "stats_metar_type" is ONLY used locally in DA_Metar:

   TYPE residual_metar1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_metar1_type

   TYPE maxmin_metar_stats_type
        TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_metar_stats_type

   TYPE stats_metar_type
        TYPE (maxmin_metar_stats_type)  :: maximum, minimum
        TYPE (residual_metar1_type)     :: average, rms_err
   END TYPE stats_metar_type

CONTAINS

#include "DA_AO_Stats_Metar.inc"
#include "DA_Calculate_Jo_and_GradY_Metar.inc"
#include "DA_Calculate_Residual_Metar.inc"
#include "DA_OI_Stats_Metar.inc"
#include "DA_Print_Stats_Metar.inc"
#include "DA_Transform_XToY_Metar.inc"
#include "DA_Transform_XToY_Metar_Adj.inc"
#include "da_check_max_iv_metar.inc"
#include "da_get_innov_vector_metar.inc"
#include "DA_Calculate_GradY_Metar.inc"


END MODULE DA_Metar

