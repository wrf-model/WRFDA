!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel WRFVAR.
!---------------------------------------------------------------------------

module da_par_util1

   use da_control
   use module_dm

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI.!
   !---------------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
#if ( DWORDsize != RWORDsize )
   integer, parameter :: true_mpi_real    = MPI_real
   integer, parameter :: true_mpi_complex = MPI_COMPLEX
   integer, parameter :: true_rsl_real    = RSL_real
#else
   integer, parameter :: true_mpi_real    = MPI_real8
   integer, parameter :: true_mpi_complex = MPI_DOUBLE_COMPLEX
   integer, parameter :: true_rsl_real    = RSL_DOUBLE
#endif
#endif

   contains

#include "da_proc_sum_int.inc"
#include "da_proc_sum_ints.inc"
#include "da_proc_sum_real.inc"

end module da_par_util1
