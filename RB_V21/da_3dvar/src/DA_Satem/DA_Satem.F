MODULE DA_Satem

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Physics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_satem_type" is ONLY used locally in DA_Satem:

   TYPE residual_satem1_type
      REAL          :: thickness                ! Satem thickness
   END TYPE residual_satem1_type

   TYPE maxmin_satem_stats_type
        TYPE (maxmin_type)         :: thickness
   END TYPE maxmin_satem_stats_type

   TYPE stats_satem_type
        TYPE (maxmin_satem_stats_type)  :: maximum, minimum
        TYPE (residual_satem1_type)     :: average, rms_err
   END TYPE stats_satem_type


CONTAINS

#include "DA_AO_Stats_Satem.inc"
#include "DA_Calculate_Jo_and_GradY_Satem.inc"
#include "DA_Calculate_Residual_Satem.inc"
#include "DA_OI_Stats_Satem.inc"
#include "DA_Print_Stats_Satem.inc"
#include "DA_Transform_XToY_Satem.inc"
#include "DA_Transform_XToY_Satem_Adj.inc"
#include "da_check_max_iv_satem.inc"
#include "da_get_innov_vector_satem.inc"
#include "DA_Calculate_GradY_Satem.inc"

END MODULE DA_Satem

