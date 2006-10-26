MODULE da_grid_definitions
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Calculation of analysis grid variables:
   !---------------------------------------------------------------------------
   
   USE da_control
   USE da_define_structures     ! For xpose_type.
   
   IMPLICIT NONE
   
   CONTAINS
   
#include "da_ref_height.inc"
#include "da_ref_pres.inc"
#include "da_ffdduv.inc"
#include "da_earth_2_model_wind.inc"
#include "da_set_map_para.inc"

END MODULE da_grid_definitions
