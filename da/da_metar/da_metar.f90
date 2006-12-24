module da_metar

  use da_control
  use da_define_structures
  use da_interpolation
  use da_statistics
  use da_tools
  use da_physics
  use da_tracing

  ! The "stats_metar_type" is ONLY used locally in da_metar:

  type residual_metar1_type
    real          :: u                        ! u-wind.
    real          :: v                        ! v-wind.
    real          :: t                        ! temperature
    real          :: p                        ! pressure
    real          :: q                        ! specific humidity
  end type residual_metar1_type

  type maxmin_metar_stats_type
     type (maxmin_type)         :: u, v, t, p, q
  end type maxmin_metar_stats_type

  type stats_metar_type
     type (maxmin_metar_stats_type)  :: maximum, minimum
     type (residual_metar1_type)     :: average, rms_err
  end type stats_metar_type

contains

#include "da_ao_stats_metar.inc"
#include "da_jo_and_grady_metar.inc"
#include "da_residual_metar.inc"
#include "da_oi_stats_metar.inc"
#include "da_print_stats_metar.inc"
#include "da_transform_xtoy_metar.inc"
#include "da_transform_xtoy_metar_adj.inc"
#include "da_check_max_iv_metar.inc"
#include "da_get_innov_vector_metar.inc"
#include "da_calculate_grady_metar.inc"


end module da_metar

