module da_ffts

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   use da_define_structures, only : xbx_type
   use da_par_util
   use module_dm, only : wrf_dm_sum_reals

   implicit none

   contains

#include "fft551.inc"
#include "fft661.inc"
#include "qpassm.inc"
#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
