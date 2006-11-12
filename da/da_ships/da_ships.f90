module da_ships

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics

   ! The "stats_ships_type" is ONLY used locally in da_ships:

   type residual_ships1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_ships1_type

   type maxmin_ships_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_ships_stats_type

   type stats_ships_type
      type (maxmin_ships_stats_type)  :: maximum, minimum
      type (residual_ships1_type)     :: average, rms_err
   end type stats_ships_type

contains

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

