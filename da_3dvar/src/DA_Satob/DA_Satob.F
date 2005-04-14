MODULE DA_Satob

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_satob_type" is ONLY used locally in DA_Satob:

   TYPE residual_satob1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_satob1_type

   TYPE maxmin_satob_stats_type
        TYPE (maxmin_type)         :: u, v
   END TYPE maxmin_satob_stats_type

   TYPE stats_satob_type
        TYPE (maxmin_satob_stats_type)  :: maximum, minimum
        TYPE (residual_satob1_type)     :: average, rms_err
   END TYPE stats_satob_type

CONTAINS

#include "DA_AO_Stats_Satob.inc"
#include "DA_Calculate_Jo_and_GradY_Satob.inc"
#include "DA_Calculate_Residual_Satob.inc"
#include "DA_OI_Stats_Satob.inc"
#include "DA_Print_Stats_Satob.inc"
#include "DA_Transform_XToY_Satob.inc"
#include "DA_Transform_XToY_Satob_Adj.inc"
#include "da_check_max_iv_satob.inc"
#include "da_get_innov_vector_satob.inc"
#include "DA_Calculate_GradY_Satob.inc"

END MODULE DA_Satob

