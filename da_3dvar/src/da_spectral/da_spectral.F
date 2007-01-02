module da_spectral

   use da_constants
   use DA_Define_Structures
   use da_fftpack5

!-----------------------------------------------------------------------
!  Contains all necessary routines to perform global spectral transform
!  (based on Fourier and Legendre decompositions). 
!-----------------------------------------------------------------------

   implicit none

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_COMPLEX   MPI_COMPLEX
#else
#define TRUE_MPI_COMPLEX   MPI_DOUBLE_COMPLEX
#endif

   CONTAINS

#include "da_asslegpol.inc"
#include "da_calc_power.inc"
#include "da_get_gausslats.inc"
#include "da_get_reglats.inc"
#include "da_initialize_h.inc"
#include "da_legtra_inv.inc"
#include "da_legtra.inc"
#include "da_setlegpol_test.inc"
#include "da_setlegpol.inc"
#include "da_test_spectral.inc"
#include "da_v_to_vv_spectral.inc"
#include "da_vv_to_v_spectral.inc"
#include "da_v_to_vv_spectral_adj.inc"
#include "da_legtra_inv_adj.inc"
#include "da_apply_power.inc"

end module da_spectral
