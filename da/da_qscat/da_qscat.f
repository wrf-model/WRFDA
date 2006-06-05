module da_qscat

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

! The "stats_qscat_type" is ONLY used locally in DA_Qscat:

   TYPE residual_qscat1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_qscat1_type

   TYPE maxmin_qscat_stats_type
        TYPE (maxmin_type)         :: u, v
   END TYPE maxmin_qscat_stats_type

   TYPE stats_qscat_type
        TYPE (maxmin_qscat_stats_type)  :: maximum, minimum
        TYPE (residual_qscat1_type)     :: average, rms_err
   END TYPE stats_qscat_type

CONTAINS

#include "da_calculate_jo_and_grady_qscat.inc"
#include "da_calculate_residual_qscat.inc"
#include "da_check_max_iv_qscat.inc"
#include "da_get_innov_vector_qscat.inc"
#include "da_oa_stats_qscat.inc"
#include "da_ob_stats_qscat.inc"
#include "da_print_stats_qscat.inc"
#include "da_transform_xtoy_qscat.inc"
#include "da_transform_xtoy_qscat_adj.inc"
#include "DA_Calculate_GradY_Qscat.inc"


end module da_qscat

