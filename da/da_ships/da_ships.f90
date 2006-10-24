MODULE da_ships

   USE da_control
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_physics

   ! The "stats_ships_type" is ONLY used locally in da_ships:

   TYPE residual_ships1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_ships1_type

   TYPE maxmin_ships_stats_type
      TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_ships_stats_type

   TYPE stats_ships_type
      TYPE (maxmin_ships_stats_type)  :: maximum, minimum
      TYPE (residual_ships1_type)     :: average, rms_err
   END TYPE stats_ships_type

CONTAINS

#include "da_ao_stats_ships.inc"
#include "da_calculate_jo_and_grady_ships.inc"
#include "da_calculate_residual_ships.inc"
#include "da_oi_stats_ships.inc"
#include "da_print_stats_ships.inc"
#include "da_transform_xtoy_ships.inc"
#include "da_transform_xtoy_ships_adj.inc"
#include "da_check_max_iv_ships.inc"
#include "da_get_innov_vector_ships.inc"
#include "da_calculate_grady_ships.inc"


end module da_ships

