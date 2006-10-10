!WRF:MODEL_LAYER:PAR_UTIL
!

! Utility subroutines for parallel WRFVAR.
!---------------------------------------------------------------------------

MODULE da_par_util

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI.
   !---------------------------------------------------------------------------

   USE da_define_structures   ! For xb_, cv_, xp_, be_, x_type definitions.
   USE da_par_util1
    
   IMPLICIT NONE

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#define TRUE_RSL_REAL     RSL_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#define TRUE_RSL_REAL     RSL_DOUBLE
#endif

#include "da_generic_typedefs.inc"

   INTERFACE da_wv_patch_to_global
      MODULE PROCEDURE da_wv_patch_to_global_2d
      MODULE PROCEDURE da_wv_patch_to_global_3d
   END INTERFACE

   CONTAINS

#include "da_cv_to_vv.inc"
#include "da_vv_to_cv.inc"
#include "da_alloc_and_copy_be_arrays.inc"
#include "da_be_local_copy.inc"
#include "da_copy_dims.inc"
#include "da_copy_tile_dims.inc"
#include "da_pack_count_obs.inc"
#include "da_unpack_count_obs.inc"
#include "da_transpose.inc"
#include "da_cv_to_global.inc"
#include "da_wv_patch_to_global_2d.inc"
#include "da_wv_patch_to_global_3d.inc"
#include "da_generic_methods.inc"
#include "da_free_global_sonde_sfc.inc"
#include "da_free_global_sound.inc"
#include "da_free_global_synop.inc"
#include "da_generic_boilerplate.inc"
#include "da_y_facade_to_global.inc"
#include "da_system.inc"
#include "da_system_4dvar.inc"

#ifdef DM_PARALLEL

#include "da_local_to_global.inc"
#include "da_proc_sum_count_obs.inc"
#include "da_proc_stats_combine.inc"
#include "da_proc_maxmin_combine.inc"

#else

#include "da_wrf_dm_interface.inc"

#endif

END MODULE da_par_util

