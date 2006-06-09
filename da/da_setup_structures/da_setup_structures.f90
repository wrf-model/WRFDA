MODULE da_setup_structures

   !---------------------------------------------------------------------------
   ! PURPOSE: Sets up various structures.
   !---------------------------------------------------------------------------

   use module_state_description
   USE da_define_structures
   USE da_constants
   USE da_grid_definitions
   USE da_obs
   USE da_ssmi
   USE da_vtox_transforms
   USE da_physics
   USE da_tools
   USE da_spectral
   use da_wrfvar_io
   use da_radiance
   USE module_wrf_error
   use da_tracing

   IMPLICIT NONE

   CONTAINS

#include "da_add_increments.inc"
#include "da_add_pbl_and_sfc_info.inc"
#include "da_get_vertical_truncation.inc"
#include "da_interpolate_regcoeff.inc"
#include "da_interpolate_stats.inc"
#include "da_rescale_background_errors.inc"
#include "da_setup_background_errors.inc"
#include "da_setup_be_global.inc"
#include "da_setup_be_regional.inc"
#include "da_setup_cv.inc"
#include "da_chgvres.inc"
#include "da_setup_firstguess.inc"
#include "da_setup_firstguess_wrf.inc"
#include "da_setup_firstguess_kma.inc"
#include "da_setup_flow_predictors.inc"
#include "da_setup_obs_structures.inc"
#include "da_setup_obs_structures_ascii.inc"
#include "da_setup_obs_structures_bufr.inc"
#include "da_setup_obs_interp_wts.inc"
#include "da_setup_runconstants.inc"
#include "da_transfer_wrftoxb.inc"
#include "da_transfer_kmatoxb.inc"
#include "da_transfer_xatowrf.inc"
#include "da_transfer_xatokma.inc"
#include "da_transfer_wrftltoxa.inc"
#include "da_transfer_wrftltoxa_adj.inc"
#include "da_transfer_xatowrftl.inc"
#include "da_transfer_xatowrftl_adj.inc"
#include "da_transfer_xatoanalysis.inc"
#include "da_cloud_model.inc"
#include "da_lcl.inc"
#include "da_cumulus.inc"
#include "da_qfrmrh.inc"
#include "da_write_increments.inc"
#include "da_write_kma_increments.inc"
#include "da_get_bins_info.inc"
#include "da_truncate_spectra.inc"

END MODULE da_setup_structures
