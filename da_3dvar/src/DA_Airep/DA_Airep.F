MODULE DA_Airep

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

CONTAINS

#include "DA_AO_Stats_Airep.inc"
#include "DA_Calculate_Jo_and_GradY_Airep.inc"
#include "DA_Calculate_Residual_Airep.inc"
#include "DA_OI_Stats_Airep.inc"
#include "DA_Print_Stats_Airep.inc"
#include "DA_Transform_XToY_Airep.inc"
#include "DA_Transform_XToY_Airep_Adj.inc"
#include "da_check_max_iv_airep.inc"
#include "da_get_innov_vector_airep.inc"
#include "DA_Calculate_GradY_Airep.inc"

END MODULE DA_Airep

