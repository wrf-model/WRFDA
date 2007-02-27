module da_etkf

!------------------------------------------------------------------------------
!  Purpose: Routines to run an ETKF within WRF-Var.
!
!  HISTORY: 11/21/2004 - Xuguang Wang's routines included in WRF-Var.Dale Barker
!------------------------------------------------------------------------------

#ifndef crayx1
   use lapack, only : dsyev
#endif

   use da_control, only : trace_use, stdout
   use da_reporting, only : da_error, message
   use da_tracing, only : da_trace_entry, da_trace_exit

   implicit none

contains

#include "da_innerprod.inc"
#include "da_matmulti.inc"
#include "da_matmultiover.inc"
#include "da_solve_etkf.inc"

end module da_etkf

