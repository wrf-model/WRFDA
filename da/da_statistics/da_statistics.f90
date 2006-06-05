MODULE da_statistics
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Contains routines used to calculates statistical quantities.
   !---------------------------------------------------------------------------
   
   USE da_define_structures
   USE da_par_util
   
   IMPLICIT NONE
   
   CONTAINS
   
#include "da_analysis_stats.inc"
#include "da_correlation_coeff1d.inc"
#include "da_correlation_coeff2d.inc"
#include "da_data_distribution.inc"
#include "da_stats_calculate.inc"

END MODULE da_statistics

