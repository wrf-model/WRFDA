MODULE DA_Satem

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Physics
   USE DA_Tools
   USE PAR_UTIL

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

