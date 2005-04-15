MODULE DA_Dynamics

!------------------------------------------------------------------------------
!  PURPOSE: Contains routines to calculate dynamical quantities.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 11/08/1999 - Creation.                           Dale Barker
!------------------------------------------------------------------------------

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_FFTs
   USE DA_Statistics
   USE DA_Interpolation
   USE DA_Tools        

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

END MODULE DA_Dynamics

