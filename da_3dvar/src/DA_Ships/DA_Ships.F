MODULE DA_Ships

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE DA_Physics
   USE PAR_UTIL

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

