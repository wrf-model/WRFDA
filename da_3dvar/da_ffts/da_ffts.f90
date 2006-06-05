MODULE da_ffts

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   USE da_define_structures
   USE par_util

   IMPLICIT NONE

   CONTAINS

#include "da_fast_cosine_transform.inc"
#include "da_fast_sine_transform.inc"
#include "da_qpassm.inc"
#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
