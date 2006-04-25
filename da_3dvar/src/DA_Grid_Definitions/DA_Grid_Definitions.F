MODULE da_grid_definitions
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Calculation of analysis grid variables:
   !---------------------------------------------------------------------------
   
   USE da_constants
   USE da_define_structures     ! For xpose_type.
   
   IMPLICIT NONE
   
   CONTAINS
   
#include <da_ref_height.inc>
#include <da_ref_pres.inc>
#include <ffdduv.inc>
#include <earth_2_model_wind.inc>
#include <set_map_para.inc>

END MODULE da_grid_definitions
