!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel WRFVAR.
!---------------------------------------------------------------------------

MODULE da_par_util1

   USE module_wrf_error
   USE da_constants

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI.!
   !---------------------------------------------------------------------------

   IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#define TRUE_RSL_REAL     RSL_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#define TRUE_RSL_REAL     RSL_DOUBLE
#endif

   CONTAINS

#include "da_proc_sum_int.inc"
#include "da_proc_sum_ints.inc"
#include "da_proc_sum_real.inc"

END MODULE da_par_util1
