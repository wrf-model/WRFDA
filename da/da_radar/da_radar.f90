module da_radar

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools

   ! The "stats_radar_type" is ONLY used locally in da_radar:

   type residual_Radar1_type
      real                    :: rv
      real                    :: rf
   end type residual_Radar1_type

   type maxmin_Radar_stats_type
      type (maxmin_type)         :: rv       ! Radial velocity
      type (maxmin_type)         :: rf       ! Reflectivity
   end type maxmin_Radar_stats_type

   type stats_Radar_type
      type (maxmin_Radar_stats_type)  :: maximum, minimum
      type (residual_Radar1_type)     :: average, rms_err
   end type stats_Radar_type

contains

#include "da_ao_stats_radar.inc"
#include "da_jo_and_grady_radar.inc"
#include "da_residual_radar.inc"
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

end module da_radar

