MODULE DA_Bogus

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

! The "stats_bogus_type" is ONLY used locally in DA_Bogus:

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

#include "DA_AO_Stats_Bogus.inc"
#include "DA_Calculate_Jo_and_GradY_Bogus.inc"
#include "DA_Calculate_Residual_Bogus.inc"
#include "DA_OI_Stats_Bogus.inc"
#include "DA_Print_Stats_Bogus.inc"
#include "DA_Transform_XToY_Bogus.inc"
#include "DA_Transform_XToY_Bogus_Adj.inc"
#include "da_check_max_iv_bogus.inc"
#include "da_get_innov_vector_bogus.inc"
#include "DA_Calculate_GradY_Bogus.inc"

END MODULE DA_Bogus

