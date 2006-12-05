module da_dynamics

   !---------------------------------------------------------------------------
   !  PURPOSE: Contains routines to calculate dynamical quantities.
   !---------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_ffts
   use da_statistics
   use da_interpolation
   use da_tools    

   implicit none

   contains

#include "da_balance_cycloterm.inc"
#include "da_balance_cycloterm_adj.inc"
#include "da_balance_cycloterm_lin.inc"
#include "da_balance_equation_adj.inc"
#include "da_balance_equation_lin.inc"
!include "da_balance_geoterm.inc"
#include "da_balance_geoterm_adj.inc"
#include "da_balance_geoterm_lin.inc"
#include "da_hydrostaticp_to_rho_adj.inc"
#include "da_hydrostaticp_to_rho_lin.inc"
#include "da_psichi_to_uv.inc"
#include "da_psichi_to_uv_adj.inc"
#include "da_uv_to_divergence.inc"
#include "da_uv_to_divergence_adj.inc"
#include "da_w_adjustment_lin.inc"
#include "da_w_adjustment_adj.inc"
#include "da_uv_to_vorticity.inc"
#include "da_wz_base.inc"           

end module da_dynamics

