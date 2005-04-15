MODULE DA_Recursive_Filter

!------------------------------------------------------------------------------
!  PURPOSE: Jim Pursers recursive filter routines.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 01/07/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   USE DA_Constants
   USE DA_Define_Structures
   USE PAR_UTIL

   IMPLICIT NONE

   CONTAINS

#include "da_perform_2drf.inc"
#include "DA_Calculate_RF_Factors.inc"
#include "DA_RF_Turning_Conditions.inc"
#include "DA_Recursive_Filter_1d.inc"
#include "DA_Recursive_Filter_1d_Adj.inc"
#include "DA_Transform_Through_RF.inc"
#include "DA_Transform_Through_RF_Adj.inc"

END MODULE DA_Recursive_Filter

