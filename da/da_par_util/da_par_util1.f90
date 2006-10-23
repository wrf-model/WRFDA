!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel WRFVAR.
!---------------------------------------------------------------------------

MODULE da_par_util1

   USE da_constants
   USE module_dm

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI.!
   !---------------------------------------------------------------------------

   IMPLICIT NONE

#ifdef DM_PARALLEL

#if ( DWORDSIZE != RWORDSIZE )
   integer, parameter :: true_mpi_real = MPI_REAL
   integer, parameter :: true_rsl_real = RSL_REAL
#else
   integer, parameter :: true_mpi_real = MPI_REAL8
   integer, parameter :: true_rsl_real = RSL_DOUBLE
#endif
#endif

   CONTAINS

#include "da_proc_sum_int.inc"
#include "da_proc_sum_ints.inc"
#include "da_proc_sum_real.inc"

END MODULE da_par_util1
