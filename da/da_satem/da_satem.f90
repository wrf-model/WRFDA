module da_satem

  use da_constants
  use da_define_structures
  use da_interpolation
  use da_statistics
  use da_physics
  use da_tools
  use da_par_util

  ! The "stats_satem_type" is ONLY used locally in DA_Satem:

  TYPE residual_satem1_type
    REAL          :: thickness                ! Satem thickness
  END TYPE residual_satem1_type

  TYPE maxmin_satem_stats_type
    TYPE (maxmin_type)         :: thickness
  END TYPE maxmin_satem_stats_type

  TYPE stats_satem_type
    TYPE (maxmin_satem_stats_type)  :: maximum, minimum
    TYPE (residual_satem1_type)     :: average, rms_err
  END TYPE stats_satem_type


CONTAINS

#include "da_ao_stats_satem.inc"
#include "da_calculate_jo_and_grady_satem.inc"
#include "da_calculate_residual_satem.inc"
#include "da_oi_stats_satem.inc"
#include "da_print_stats_satem.inc"
#include "da_transform_xtoy_satem.inc"
#include "da_transform_xtoy_satem_adj.inc"
#include "da_check_max_iv_satem.inc"
#include "da_get_innov_vector_satem.inc"
#include "da_calculate_grady_satem.inc"

end module da_satem

