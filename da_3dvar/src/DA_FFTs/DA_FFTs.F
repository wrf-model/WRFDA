MODULE da_ffts

   !---------------------------------------------------------------------------
   ! PURPOSE: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   USE da_define_structures
   USE par_util

   IMPLICIT NONE

   CONTAINS

#include "DA_Fast_Cosine_Transform.inc"
#include "DA_Fast_Sine_Transform.inc"
#include "DA_QPASSM.inc"
#include "DA_Solve_PoissonEqn_FCT.inc"
#include "DA_Solve_PoissonEqn_FCT_Adj.inc"
#include "DA_Solve_PoissonEqn_FST.inc"
#include "DA_Solve_PoissonEqn_FST_Adj.inc"

END MODULE da_ffts
