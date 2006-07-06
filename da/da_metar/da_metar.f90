module da_metar

  use da_constants
  use da_define_structures
  use da_interpolation
  use da_statistics
  use da_tools
  use da_physics
  use da_par_util
  use da_tracing

  ! The "stats_metar_type" is ONLY used locally in da_metar:

  TYPE residual_metar1_type
    REAL          :: u                        ! u-wind.
    REAL          :: v                        ! v-wind.
    REAL          :: t                        ! temperature
    REAL          :: p                        ! pressure
    REAL          :: q                        ! specific humidity
  END TYPE residual_metar1_type

  TYPE maxmin_metar_stats_type
     TYPE (maxmin_type)         :: u, v, t, p, q
  END TYPE maxmin_metar_stats_type

  TYPE stats_metar_type
     TYPE (maxmin_metar_stats_type)  :: maximum, minimum
     TYPE (residual_metar1_type)     :: average, rms_err
  END TYPE stats_metar_type

CONTAINS

#include "da_ao_stats_metar.inc"
#include "da_calculate_jo_and_grady_metar.inc"
#include "da_calculate_residual_metar.inc"
#include "da_oi_stats_metar.inc"
#include "da_print_stats_metar.inc"
#include "da_transform_xtoy_metar.inc"
#include "da_transform_xtoy_metar_adj.inc"
#include "da_check_max_iv_metar.inc"
#include "da_get_innov_vector_metar.inc"
#include "da_calculate_grady_metar.inc"


end module da_metar

