module da_grid_definitions
   
   !---------------------------------------------------------------------------
   ! Purpose: Calculation of analysis grid variables:
   !---------------------------------------------------------------------------
   
   use da_control
   use da_define_structures     ! For xpose_type.
   
   implicit none
   
   contains
   
#include "da_ref_height.inc"
#include "da_ref_pres.inc"
#include "da_ffdduv.inc"
#include "da_earth_2_model_wind.inc"
#include "da_set_map_para.inc"

end module da_grid_definitions
