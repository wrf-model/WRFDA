module da_ffts

   !---------------------------------------------------------------------------
   ! Purpose: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   use da_define_structures, only : xbx_type
   use da_par_util
   use module_dm, only : wrf_dm_sum_reals
   use module_ffts

   implicit none

   contains

#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
