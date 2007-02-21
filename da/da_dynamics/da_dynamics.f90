module da_dynamics

   !---------------------------------------------------------------------------
   !  Purpose: Contains routines to calculate dynamical quantities.
   !---------------------------------------------------------------------------

   use da_control, only : ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
      gamma, gravity, ids,ide,jds,jde,kds,kde, global, Testing_WRFVAR, &
      fg_format, fg_format_kma_global, balance_geo,balance_geocyc, fg_format_wrf, &
      balance_type, balance_cyc, gravity, convert_fd2uv
   use module_domain, only : xb_type,xpose_type
   use da_define_structures, only : xbx_type
   use da_reporting, only : message, da_error   
   use da_ffts, only : da_solve_poissoneqn_fst, da_solve_poissoneqn_fst_adj
   use da_tools, only : da_set_boundary_3d

   implicit none

   contains

#include "da_balance_cycloterm.inc"
#include "da_balance_cycloterm_adj.inc"
#include "da_balance_cycloterm_lin.inc"
#include "da_balance_equation_adj.inc"
#include "da_balance_equation_lin.inc"
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

