module da_airep

  use da_constants
  use da_define_structures
  use da_interpolation
  use da_statistics
  use da_tools
  use par_util

  ! The "stats_airep_type" is ONLY used locally in DA_Airep:

  TYPE residual_airep1_type
    REAL          :: u                        ! u-wind.
    REAL          :: v                        ! v-wind.
    REAL          :: t                        ! temperature
  END TYPE residual_airep1_type

  TYPE maxmin_airep_stats_type
    TYPE (maxmin_type)         :: u, v, t 
  END TYPE maxmin_airep_stats_type

  TYPE stats_airep_type
    TYPE (maxmin_airep_stats_type)  :: maximum, minimum
    TYPE (residual_airep1_type)     :: average, rms_err
  END TYPE stats_airep_type

CONTAINS

#include "da_ao_stats_airep.inc"
#include "da_calculate_jo_and_grady_airep.inc"
#include "da_calculate_residual_airep.inc"
#include "da_oi_stats_airep.inc"
#include "da_print_stats_airep.inc"
#include "da_transform_xtoy_airep.inc"
#include "da_transform_xtoy_airep_adj.inc"
#include "da_check_max_iv_airep.inc"
#include "da_get_innov_vector_airep.inc"
#include "da_calculate_grady_airep.inc"

end module da_airep

