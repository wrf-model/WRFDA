MODULE DA_Radar

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Interpolation
   USE DA_Statistics
   USE DA_Tools

CONTAINS

#include "DA_AO_Stats_Radar.inc"
#include "DA_Calculate_Jo_and_GradY_Radar.inc"
#include "DA_Calculate_Residual_Radar.inc"
#include "DA_OI_Stats_Radar.inc"
#include "DA_Print_Stats_Radar.inc"
#include "DA_Transform_XToY_Radar.inc"
#include "DA_Transform_XToY_Radar_Adj.inc"
#include "da_check_max_iv_Radar.inc"
#include "da_get_innov_vector_Radar.inc"
#include "da_radial_velocity.inc"
#include "da_radial_velocity_Lin.inc"
#include "da_radial_velocity_Adj.inc"
#include "da_reflectivity.inc"
#include "da_reflectivity_Lin.inc"
#include "da_reflectivity_Adj.inc"
#include "DA_Calculate_GradY_Radar.inc"
#include "da_max_error_qc_Radar.inc"

END MODULE DA_Radar

