module da_etkf

!------------------------------------------------------------------------------
!  Purpose: Routines to run an ETKF within WRF-Var.
!
!  HISTORY: 11/21/2004 - Xuguang Wang's routines included in WRF-Var.Dale Barker
!------------------------------------------------------------------------------

   use da_control
   use da_tracing
   use lapack, only : dsyev

   implicit none

contains

#include "da_innerprod.inc"
#include "da_matmulti.inc"
#include "da_matmultiover.inc"
#include "da_solve_etkf.inc"

end module da_etkf

