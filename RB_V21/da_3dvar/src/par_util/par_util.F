!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel 3DVAR.
!---------------------------------------------------------------------------

MODULE PAR_UTIL

!------------------------------------------------------------------------------
!  PURPOSE: Routines for local-to-global and global-to-local grid operations.
!
!  METHOD:  RSL/MPI.
!
!  HISTORY: 10/25/2001 - Creation of F90 version.           Al Bourgeois
!
!------------------------------------------------------------------------------

    USE DA_Define_Structures   ! For xb_, cv_, xp_, be_, x_type definitions.
    USE DA_Constants           ! print_detail
    
    IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#define TRUE_RSL_REAL     RSL_REAL
#define TRUE_RSL_REAL_F90 RSL_REAL_F90
#else
#define TRUE_MPI_REAL     MPI_REAL8
#define TRUE_RSL_REAL     RSL_DOUBLE
#define TRUE_RSL_REAL_F90 RSL_DOUBLE_F90
#endif

    CONTAINS

#include <cv_to_vv.inc>
#include <vv_to_cv.inc>
#include <alloc_and_copy_be_arrays.inc>
#include <be_local_copy.inc>
#include <calculate_cv_local_size.inc>
#include <copy_dims.inc>
#include <copy_tile_dims.inc>
#include <pack_count_obs.inc>
#include <unpack_count_obs.inc>
#include <transpose.inc>

#ifdef DM_PARALLEL

#include <local_to_global.inc>
#include <mm5_struct_bcast.inc>
#include <proc_sum_int.inc>
#include <proc_sum_real.inc>
#include <proc_sum_count_obs.inc>
#include <proc_stats_combine.inc>
#include <proc_maxmin_combine.inc>

#else

#include <wrf_dm_interface.inc>

#endif

END MODULE PAR_UTIL

