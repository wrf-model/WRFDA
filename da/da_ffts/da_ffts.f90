module da_ffts

   !---------------------------------------------------------------------------
   ! Purpose: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   use module_domain, only : xpose_type
   use da_control, only : ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
      Inverse_FFT,  Forward_FFT, ide,jde, stdout
   use da_define_structures, only : xbx_type
   use da_par_util, only : da_transpose_x2z, da_transpose_y2x, &
      da_transpose_y2x_v2, da_transpose_z2x, da_transpose_x2y, &
      da_transpose_x2y_v2
   use module_dm, only : wrf_dm_sum_reals
   use module_ffts, only : fft661, fft551
   use da_wrf_interfaces, only : wrf_dm_halo

   implicit none

   contains

#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
