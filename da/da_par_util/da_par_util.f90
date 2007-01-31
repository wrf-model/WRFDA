module da_par_util

   !---------------------------------------------------------------------------
   ! Purpose: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI.
   !---------------------------------------------------------------------------

   use module_domain, only : domain, xpose_type
#ifdef DM_PARALLEL
   use module_dm, only : io2d_ij_internal, io3d_ijk_internal 
   use mpi, only : mpi_2double_precision, mpi_status_size, &
      mpi_integer, mpi_maxloc, mpi_status_size, &
      mpi_minloc, mpi_sum
#endif
   use da_define_structures, only : be_subtype, &
      x_type, vp_type, residual_synop_type, residual_sound_type, ob_type, &
      y_type, count_obs_number_type, count_obs_type, maxmin_field_type
#ifdef DM_PARALLEL
   use da_par_util1, only : true_mpi_real, true_mpi_complex, true_rsl_real, &
      true_mpi_real, true_mpi_complex, true_rsl_real
#endif
   use da_control, only : trace_use,num_ob_indexes, myproc, root, comm, ierr, &
      rootproc, num_procs, stdout, print_detail_parallel, its,ite, jts, jte, &
      kts,kte,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe, &
      kps, kpe
   use da_tools, only : message
   use da_reporting, only : da_error
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_wrf_interfaces, only : wrf_dm_bcast_integer, &
      wrf_dm_xpose_z2x,wrf_dm_xpose_x2y, wrf_dm_xpose_y2x, wrf_dm_xpose_x2z, &
      wrf_dm_xpose_z2y, wrf_dm_xpose_y2z, wrf_patch_to_global_real

   implicit none

#include "da_generic_typedefs.inc"

   interface da_wv_patch_to_global
      module procedure da_wv_patch_to_global_2d
      module procedure da_wv_patch_to_global_3d
   end interface

   contains

#include "da_cv_to_vv.inc"
#include "da_vv_to_cv.inc"
#include "da_alloc_and_copy_be_arrays.inc"
#include "da_copy_dims.inc"
#include "da_copy_tile_dims.inc"
#include "da_pack_count_obs.inc"
#include "da_unpack_count_obs.inc"
#include "da_transpose.inc"
#include "da_cv_to_global.inc"
#include "da_wv_patch_to_global_2d.inc"
#include "da_wv_patch_to_global_3d.inc"
#include "da_generic_methods.inc"
#include "da_deallocate_global_sonde_sfc.inc"
#include "da_deallocate_global_sound.inc"
#include "da_deallocate_global_synop.inc"
#include "da_generic_boilerplate.inc"
#include "da_y_facade_to_global.inc"
#include "da_system.inc"

#ifdef DM_PARALLEL

#include "da_local_to_global.inc"
#include "da_proc_sum_count_obs.inc"
#include "da_proc_stats_combine.inc"
#include "da_proc_maxmin_combine.inc"

#else

#include "da_wrf_dm_interface.inc"

#endif

end module da_par_util

