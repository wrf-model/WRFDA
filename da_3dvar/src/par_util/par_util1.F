!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel WRFVAR.
!---------------------------------------------------------------------------

MODULE PAR_UTIL1

!------------------------------------------------------------------------------
!  PURPOSE: Routines for local-to-global and global-to-local grid operations.
!
!  METHOD:  RSL/MPI.
!
!  HISTORY: 10/25/2001 - Creation of F90 version.           Al Bourgeois
!
!------------------------------------------------------------------------------

    IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#define TRUE_RSL_REAL     RSL_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#define TRUE_RSL_REAL     RSL_DOUBLE
#endif

!#include <generic_typedefs.inc>

    CONTAINS

#include <proc_sum_int.inc>
#include <proc_sum_real.inc>

END MODULE PAR_UTIL1

