module da_setup_structures

   !---------------------------------------------------------------------------
   ! Purpose: Sets up various structures.
   !---------------------------------------------------------------------------

   use module_state_description
   use da_define_structures
   use da_grid_definitions
   use da_obs
   use da_obs_io
   use da_ssmi
   use da_vtox_transforms
   use da_spectral
   use da_wrfvar_io
   use da_radiance
   use da_reporting

   implicit none

   contains

#include "da_add_pbl_and_sfc_info.inc"
#include "da_get_vertical_truncation.inc"
#include "da_interpolate_regcoeff.inc"
!#include "da_interpolate_stats.inc"
#include "da_rescale_background_errors.inc"
#include "da_setup_background_errors.inc"
#include "da_setup_be_global.inc"
#include "da_setup_be_regional.inc"
#include "da_setup_cv.inc"
#include "da_chgvres.inc"
#include "da_setup_flow_predictors.inc"
#include "da_setup_obs_structures.inc"
#include "da_setup_obs_structures_ascii.inc"
#include "da_setup_obs_structures_bufr.inc"
#include "da_setup_obs_interp_wts.inc"
#include "da_setup_runconstants.inc"
#include "da_cloud_model.inc"
#include "da_lcl.inc"
#include "da_cumulus.inc"
#include "da_qfrmrh.inc"
#include "da_write_increments.inc"
#include "da_write_kma_increments.inc"
#include "da_get_bins_info.inc"
#include "da_truncate_spectra.inc"

end module da_setup_structures
