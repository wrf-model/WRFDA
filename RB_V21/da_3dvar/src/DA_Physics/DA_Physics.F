MODULE DA_Physics

!------------------------------------------------------------------------------
!  PURPOSE: Contains routines to calculate physical quantities.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 10/03/2000 - Creation.                           Dale Barker
!------------------------------------------------------------------------------

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Grid_Definitions
   USE DA_Interpolation
   USE DA_Dynamics

   IMPLICIT NONE

   CONTAINS

#include "DA_PRho_To_T_Adj.inc"
#include "DA_PRho_To_T_Lin.inc"
#include "DA_UVPRho_To_W_Lin.inc"
#include "DA_UVPRho_To_W_Adj.inc"
#include "DA_PT_To_Rho_Adj.inc"
#include "DA_PT_To_Rho_Lin.inc"
#include "DA_TPQ_To_RH.inc"
#include "DA_TPQ_To_RH_Lin.inc"
#include "DA_TPRH_To_Q_Adj.inc"
#include "DA_TPRH_To_Q_Lin.inc"
#include "DA_TP_To_Qs.inc"
#include "DA_TP_To_Qs_Adj.inc"
#include "DA_TP_To_Qs_Lin.inc"
#include "DA_TRH_To_TD.inc"
#include "DA_TPQ_To_SLP.inc"
#include "DA_TPQ_To_SLP_Lin.inc"
#include "DA_TPQ_To_SLP_Adj.inc"
#include "DA_TPQ_To_Thickness.inc"
#include "DA_Transform_XToTPW.inc"
#include "DA_Transform_XToTPW_Adj.inc"
#include "DA_Transform_XToGPSRef.inc"
#include "DA_Transform_XToGPSRef_Adj.inc"
#include "DA_Transform_XToGPSRef_Lin.inc"
#include "da_check_rh.inc"
#include "da_check_rh_simple.inc"
#include "da_e_qv_from_rh.inc"
#include "da_get_q_error.inc"
#include "roughness_from_lanu.inc"
#include "da_sfc_wtq.inc"
#include "da_sfc_wtq_Lin.inc"
#include "da_sfc_wtq_Adj.inc"
#include "DA_Transform_XToPsfc.inc"
#include "DA_Transform_XToPsfc_Adj.inc"
#include "DA_Transform_XToWTQ.inc"
#include "DA_Transform_XToWTQ_Adj.inc"
#include "da_sfc_pre.inc"
#include "da_sfc_pre_Lin.inc"
#include "da_sfc_pre_Adj.inc"
#include "DA_Moist_Phys_Adj.inc"
#include "DA_Moist_Phys_Lin.inc"
#include "da_accre.inc"
#include "da_accre_adj.inc"
#include "da_accre_lin.inc"
#include "da_autoc.inc"
#include "da_autoc_adj.inc"
#include "da_autoc_lin.inc"
#include "da_condens_adj.inc"
#include "da_condens_lin.inc"
#include "da_evapo.inc"
#include "da_evapo_adj.inc"
#include "da_evapo_lin.inc"
#include "da_filter.inc"
#include "da_filter_adj.inc"
#include "da_wdt.inc"

END MODULE DA_Physics

