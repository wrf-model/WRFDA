module da_buoy 

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics

   ! The "stats_buoy_type" is ONLY used locally in da_buoy:

   type residual_buoy1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_buoy1_type

   type maxmin_buoy_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_buoy_stats_type

   type stats_buoy_type
      type (maxmin_buoy_stats_type)  :: maximum, minimum
      type (residual_buoy1_type)     :: average, rms_err
   end type stats_buoy_type

contains

#include "da_ao_stats_buoy.inc"
#include "da_calculate_jo_and_grady_buoy.inc"
#include "da_calculate_residual_buoy.inc"
#include "da_oi_stats_buoy.inc"
#include "da_print_stats_buoy.inc"
#include "da_transform_xtoy_buoy.inc"
#include "da_transform_xtoy_buoy_adj.inc"
#include "da_check_max_iv_buoy.inc"
#include "da_get_innov_vector_buoy.inc"
#include "da_calculate_grady_buoy.inc"


end module da_buoy 

