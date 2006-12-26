module da_recursive_filter

   !---------------------------------------------------------------------------
   ! Purpose: Jim Pursers recursive filter routines.
   !---------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_par_util

   implicit none

   contains

#include "da_perform_2drf.inc"
#include "da_calculate_rf_factors.inc"
#include "da_recursive_filter_1d.inc"
#include "da_recursive_filter_1d_adj.inc"
#include "da_transform_through_rf.inc"
#include "da_transform_through_rf_adj.inc"

end module da_recursive_filter

