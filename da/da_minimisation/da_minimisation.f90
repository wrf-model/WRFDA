module da_minimisation

   !---------------------------------------------------------------------------
   ! Purpose: Collection of routines associated with minimisation. 
   !---------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_vtox_transforms
   use da_obs
   use da_metar
   use da_geoamv
   use da_polaramv
   use da_ships
   use da_synop
   use da_sound
   use da_airep
   use da_pilot
   use da_gpspw
   use da_gpsref
   use da_ssmi
   use da_satem
   use da_pseudo
   use da_bogus
   use da_profiler
   use da_buoy 
   use da_setup_structures
   use da_qscat
   use da_radiance
   use da_airsr     
   use module_get_file_names ! for system call on cray
   use da_wrfvar_io

   implicit none

   private :: da_dot, da_dot_cv

contains
      
#include "da_calculate_j.inc"
#include "da_jo_and_grady.inc"
#include "da_calculate_residual.inc"
#include "da_get_var_diagnostics.inc"
#include "da_get_innov_vector.inc"
#include "da_sum_reals.inc"
#include "da_dot.inc"
#include "da_dot_cv.inc"
#include "da_write_diagnostics.inc"
#include "da_minimise_cg.inc"
#include "da_calculate_grady.inc"
#include "da_transform_vtoy.inc"
#include "da_transform_vtoy_adj.inc"

end module da_minimisation
