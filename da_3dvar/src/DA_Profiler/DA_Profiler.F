MODULE DA_Profiler

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools
   USE PAR_UTIL

CONTAINS

#include "DA_AO_Stats_Profiler.inc"
#include "DA_Calculate_Jo_and_GradY_Profiler.inc"
#include "DA_Calculate_Residual_Profiler.inc"
#include "DA_OI_Stats_Profiler.inc"
#include "DA_Print_Stats_Profiler.inc"
#include "DA_Transform_XToY_Profiler.inc"
#include "DA_Transform_XToY_Profiler_Adj.inc"
#include "da_check_max_iv_profiler.inc"
#include "da_get_innov_vector_profiler.inc"
#include "DA_Calculate_GradY_Profiler.inc"


END MODULE DA_Profiler

