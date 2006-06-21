MODULE da_radar

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools

   ! The "stats_radar_type" is ONLY used locally in da_radar:

   TYPE residual_Radar1_type
      REAL                    :: rv
      REAL                    :: rf
   END TYPE residual_Radar1_type

   TYPE maxmin_Radar_stats_type
      TYPE (maxmin_type)         :: rv       ! Radial velocity
      TYPE (maxmin_type)         :: rf       ! Reflectivity
   END TYPE maxmin_Radar_stats_type

   TYPE stats_Radar_type
      TYPE (maxmin_Radar_stats_type)  :: maximum, minimum
      TYPE (residual_Radar1_type)     :: average, rms_err
   END TYPE stats_Radar_type

CONTAINS

#include "da_ao_stats_radar.inc"
#include "da_calculate_jo_and_grady_radar.inc"
#include "da_calculate_residual_radar.inc"
#include "da_oi_stats_radar.inc"
#include "da_print_stats_radar.inc"
#include "da_transform_xtoy_radar.inc"
#include "da_transform_xtoy_radar_adj.inc"
#include "da_check_max_iv_radar.inc"
#include "da_get_innov_vector_radar.inc"
#include "da_radial_velocity.inc"
#include "da_radial_velocity_lin.inc"
#include "da_radial_velocity_adj.inc"
#include "da_reflectivity.inc"
#include "da_reflectivity_lin.inc"
#include "da_reflectivity_adj.inc"
#include "da_calculate_grady_radar.inc"
#include "da_max_error_qc_radar.inc"

END MODULE da_radar

