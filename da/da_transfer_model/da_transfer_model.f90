module da_transfer_model
use da_tools1

   !---------------------------------------------------------------------------
   ! Purpose: Transfer model states between different models
   !---------------------------------------------------------------------------

   use module_domain
   use da_setup_structures
   use module_configure
   use module_state_description

#ifdef DM_PARALLEL
   use module_dm
#endif

   use da_control
   use da_define_structures
   use da_grid_definitions
   use da_par_util
   use da_physics
   use da_reporting
   use da_setup_structures
   use da_ssmi
   use da_tools
   use da_tracing
   use da_vtox_transforms
!   use da_wrf_interfaces
   use da_wrfvar_io

   implicit none

   contains

#include "da_transfer_wrftoxb.inc"
#include "da_transfer_kmatoxb.inc"
#include "da_transfer_xatowrf.inc"
#include "da_transfer_xatokma.inc"
#include "da_transfer_wrftltoxa.inc"
#include "da_transfer_wrftltoxa_adj.inc"
#include "da_transfer_xatowrftl.inc"
#include "da_transfer_xatowrftl_adj.inc"
#include "da_transfer_xatoanalysis.inc"
#include "da_setup_firstguess.inc"
#include "da_setup_firstguess_wrf.inc"
#include "da_setup_firstguess_kma.inc"

end module da_transfer_model
