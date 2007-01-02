MODULE DA_Statistics
   
!---- -------------------------------------------------------------------------
!     PURPOSE: Contains routines used to calculates statistical quantities.
!
!     METHOD:  See individual routines.
!
!     HISTORY: 09/28/1999 - Creation of F90 version.        Dale Barker
!------------------------------------------------------------------------------
   
   USE DA_Define_Structures
   USE PAR_UTIL
   
   IMPLICIT NONE
   
   CONTAINS
   
#include "DA_Analysis_Stats.inc"
#include "DA_Correlation_Coeff1d.inc"
#include "DA_Correlation_Coeff2d.inc"
#include "DA_Data_Distribution.inc"
#include "Stats_Calculate.inc"

END MODULE DA_Statistics

