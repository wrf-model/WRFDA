MODULE da_recursive_filter

   !---------------------------------------------------------------------------
   ! PURPOSE: Jim Pursers recursive filter routines.
   !---------------------------------------------------------------------------

   use da_constants
   use da_define_structures
   use da_par_util

   IMPLICIT NONE

   CONTAINS

#include "da_perform_2drf.inc"
#include "da_calculate_rf_factors.inc"
#include "da_recursive_filter_1d.inc"
#include "da_recursive_filter_1d_adj.inc"
#include "da_transform_through_rf.inc"
#include "da_transform_through_rf_adj.inc"

end module da_recursive_filter

