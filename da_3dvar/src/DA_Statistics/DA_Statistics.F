MODULE da_statistics
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Contains routines used to calculates statistical quantities.
   !---------------------------------------------------------------------------
   
   USE da_define_structures
   USE par_util
   
   IMPLICIT NONE
   
   CONTAINS
   
#include "DA_Analysis_Stats.inc"
#include "DA_Correlation_Coeff1d.inc"
#include "DA_Correlation_Coeff2d.inc"
#include "DA_Data_Distribution.inc"
#include "Stats_Calculate.inc"

END MODULE da_statistics

