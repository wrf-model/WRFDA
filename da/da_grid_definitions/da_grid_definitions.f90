module da_grid_definitions
   
   !---------------------------------------------------------------------------
   ! Purpose: Calculation of analysis grid variables:
   !---------------------------------------------------------------------------
   
   use da_control, only : gravity, convert_fd2uv, gas_constant, convert_uv2fd, &
      pi, cone_factor, map_projection, phic,psi1,earth_radius,c2,pole, &
      ycntr,truelat1_3dv,xlonc,ptop,t0,base_pres,base_lapse,base_temp
   use da_reporting, only : da_error,message
   
   implicit none
   
   contains
   
#include "da_ref_height.inc"
#include "da_ref_pres.inc"
#include "da_ffdduv.inc"
#include "da_earth_2_model_wind.inc"
#include "da_set_map_para.inc"

end module da_grid_definitions
