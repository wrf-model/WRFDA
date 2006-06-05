MODULE da_pilot

  use da_constants
  use da_define_structures
  use da_interpolation
  use da_statistics
  use da_tools
  use par_util

  ! The "stats_pilot_type" is ONLY used locally in DA_Pilot:

  TYPE residual_pilot1_type
    REAL          :: u                        ! u-wind.
    REAL          :: v                        ! v-wind.
  END TYPE residual_pilot1_type

  TYPE maxmin_pilot_stats_type
    TYPE (maxmin_type)         :: u, v
  END TYPE maxmin_pilot_stats_type

  TYPE stats_pilot_type
    TYPE (maxmin_pilot_stats_type)  :: maximum, minimum
    TYPE (residual_pilot1_type)     :: average, rms_err
  END TYPE stats_pilot_type

CONTAINS

#include "da_ao_stats_pilot.inc"
#include "da_calculate_jo_and_grady_pilot.inc"
#include "da_calculate_residual_pilot.inc"
#include "da_oi_stats_pilot.inc"
#include "da_print_stats_pilot.inc"
#include "da_transform_xtoy_pilot.inc"
#include "da_transform_xtoy_pilot_adj.inc"
#include "da_check_max_iv_pilot.inc"
#include "da_get_innov_vector_pilot.inc"
#include "da_calculate_grady_pilot.inc"

end module da_pilot

