MODULE DA_FFTs

!------------------------------------------------------------------------------
!  PURPOSE: Routines to perform Fourier transforms.
!
!  METHOD:  Varied.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!------------------------------------------------------------------------------

   USE DA_Define_Structures
   USE PAR_UTIL

   IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

   CONTAINS

#include "DA_Fast_Cosine_Transform.inc"
#include "DA_Fast_Sine_Transform.inc"
#include "DA_QPASSM.inc"
#include "DA_Solve_PoissonEqn_FCT.inc"
#include "DA_Solve_PoissonEqn_FCT_Adj.inc"
#include "DA_Solve_PoissonEqn_FST.inc"
#include "DA_Solve_PoissonEqn_FST_Adj.inc"

END MODULE DA_FFTs
