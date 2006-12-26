module be_spectral

   use da_control
   use da_tools1
   use fftpack5
   use da_tracing

   !--------------------------------------------------------------------
   ! Contains all necessary routines to perform global spectral transform
   !  (based on Fourier and Legendre decompositions).  
   !--------------------------------------------------------------------

   implicit none

contains

#include "da_asslegpol.inc"
#include "da_calc_power.inc"
#include "da_get_gausslats.inc"
#include "da_get_reglats.inc"
#include "da_initialize_h.inc"
#include "da_legtra_inv.inc"
#include "da_legtra.inc"
#include "da_setlegpol_test.inc"
#include "da_setlegpol.inc"
#include "da_vv_to_v_spectral.inc"
#include "da_legtra_inv_adj.inc"
#include "da_apply_power.inc"

end module be_spectral

