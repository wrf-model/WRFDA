MODULE da_recursive_filter

!------------------------------------------------------------------------------
!  PURPOSE: Jim Pursers recursive filter routines.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 01/07/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   use da_constants
   use da_define_structures
   use par_util
   use module_wrf_error

   IMPLICIT NONE

   CONTAINS

#include "da_perform_2drf.inc"
#include "da_calculate_rf_factors.inc"
#include "da_rf_turning_conditions.inc"
#include "da_recursive_filter_1d.inc"
#include "da_recursive_filter_1d_adj.inc"
#include "da_transform_through_rf.inc"
#include "da_transform_through_rf_adj.inc"

end module da_recursive_filter

