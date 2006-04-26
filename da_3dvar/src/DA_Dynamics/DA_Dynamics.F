MODULE da_dynamics

   !---------------------------------------------------------------------------
   !  PURPOSE: Contains routines to calculate dynamical quantities.
   !---------------------------------------------------------------------------

   USE da_constants
   USE da_define_structures
   USE da_ffts
   USE da_statistics
   USE da_interpolation
   USE da_tools    
   use module_wrf_error    

   IMPLICIT NONE

   CONTAINS

#include "DA_Balance_CycloTerm.inc"
#include "DA_Balance_CycloTerm_Adj.inc"
#include "DA_Balance_CycloTerm_Lin.inc"
#include "DA_Balance_Equation_Adj.inc"
#include "DA_Balance_Equation_Lin.inc"
!include "DA_Balance_GeoTerm.inc"
#include "DA_Balance_GeoTerm_Adj.inc"
#include "DA_Balance_GeoTerm_Lin.inc"
#include "DA_HydrostaticP_To_Rho_Adj.inc"
#include "DA_HydrostaticP_To_Rho_Lin.inc"
#include "DA_PsiChi_To_UV.inc"
#include "DA_PsiChi_To_UV_Adj.inc"
#include "DA_UV_To_Divergence.inc"
#include "DA_UV_To_Divergence_Adj.inc"
#include "DA_W_Adjustment_Lin.inc"
#include "DA_W_Adjustment_Adj.inc"
#include "DA_UV_To_Vorticity.inc"
#include "DA_WZ_BASE.inc"           

END MODULE da_dynamics

