module da_statistics
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Contains routines used to calculates statistical quantities.
   !---------------------------------------------------------------------------
   
   use module_domain, only : xpose_type
   use da_control
   use da_define_structures, only : maxmin_type, x_type, maxmin_field_type
   use da_par_util1
   use da_par_util
   use da_tracing
   
   implicit none
   
   contains
   
#include "da_analysis_stats.inc"
#include "da_correlation_coeff1d.inc"
#include "da_correlation_coeff2d.inc"
#include "da_data_distribution.inc"
#include "da_stats_calculate.inc"

end module da_statistics

