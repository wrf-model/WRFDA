module da_ffts

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   use da_define_structures
   use da_par_util

   implicit none

   contains

#include "FFT551.inc"
#include "FFT661.inc"
#include "qpassm.inc"
#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
