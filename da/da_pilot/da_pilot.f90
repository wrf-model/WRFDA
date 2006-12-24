module da_pilot

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_pilot_type" is ONLY used locally in da_pilot:

   type residual_pilot1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_pilot1_type

   type maxmin_pilot_stats_type
      type (maxmin_type)         :: u, v
   end type maxmin_pilot_stats_type

   type stats_pilot_type
      type (maxmin_pilot_stats_type)  :: maximum, minimum
      type (residual_pilot1_type)     :: average, rms_err
   end type stats_pilot_type

contains

#include "da_ao_stats_pilot.inc"
#include "da_jo_and_grady_pilot.inc"
#include "da_residual_pilot.inc"
#include "da_oi_stats_pilot.inc"
#include "da_print_stats_pilot.inc"
#include "da_transform_xtoy_pilot.inc"
#include "da_transform_xtoy_pilot_adj.inc"
#include "da_check_max_iv_pilot.inc"
#include "da_get_innov_vector_pilot.inc"
#include "da_calculate_grady_pilot.inc"

end module da_pilot

