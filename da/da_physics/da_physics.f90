MODULE da_physics

   !---------------------------------------------------------------------------
   !  PURPOSE: Contains routines to calculate physical quantities.
   !---------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_grid_definitions
   use da_interpolation
   use da_dynamics
   use da_reporting
   use module_wrf_error

   IMPLICIT NONE

   CONTAINS

#include "da_prho_to_t_adj.inc"
#include "da_prho_to_t_lin.inc"
#include "da_uvprho_to_w_lin.inc"
#include "da_uvprho_to_w_adj.inc"
#include "da_pt_to_rho_adj.inc"
#include "da_pt_to_rho_lin.inc"
#include "da_tpq_to_rh.inc"
#include "da_tpq_to_rh_lin.inc"
#include "da_tpq_to_rh_lin1.inc"
#include "da_tprh_to_q_adj.inc"
#include "da_tprh_to_q_adj1.inc"
#include "da_tprh_to_q_lin.inc"
#include "da_tprh_to_q_lin1.inc"
#include "da_tp_to_qs.inc"
#include "da_tp_to_qs1.inc"
#include "da_tp_to_qs_adj.inc"
#include "da_tp_to_qs_adj1.inc"
#include "da_tp_to_qs_lin.inc"
#include "da_tp_to_qs_lin1.inc"
#include "da_trh_to_td.inc"
#include "da_tpq_to_slp.inc"
#include "da_wrf_tpq_2_slp.inc"
#include "da_tpq_to_slp_lin.inc"
#include "da_tpq_to_slp_adj.inc"
#include "da_tpq_to_thickness.inc"
#include "da_transform_xtotpw.inc"
#include "da_transform_xtotpw_adj.inc"
#include "da_transform_xtogpsref.inc"
#include "da_transform_xtogpsref_adj.inc"
#include "da_transform_xtogpsref_lin.inc"
#include "da_check_rh.inc"
#include "da_check_rh_simple.inc"
#include "da_e_qv_from_rh.inc"
#include "da_get_q_error.inc"
#include "da_roughness_from_lanu.inc"
#include "da_julian_day.inc"
#include "da_sfc_wtq.inc"
#include "da_sfc_wtq_lin.inc"
#include "da_sfc_wtq_adj.inc"
#include "da_transform_xtopsfc.inc"
#include "da_transform_xtopsfc_adj.inc"
#include "da_transform_xtowtq.inc"
#include "da_transform_xtowtq_adj.inc"
#include "da_sfc_pre.inc"
#include "da_sfc_pre_lin.inc"
#include "da_sfc_pre_adj.inc"
#include "da_moist_phys_adj.inc"
#include "da_moist_phys_lin.inc"
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
#include "da_integrat_dz.inc"

END MODULE da_physics

