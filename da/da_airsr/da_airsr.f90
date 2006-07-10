MODULE da_airsr

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_physics
   USE da_par_util

   ! The "stats_airsr_type" is ONLY used locally in da_airsr:

   TYPE residual_airsr1_type
      REAL          :: t                        ! temperature
      REAL          :: q                        ! specific humidity
   END TYPE residual_airsr1_type

   TYPE maxmin_airsr_stats_type
      TYPE (maxmin_type)         :: t, q
   END TYPE maxmin_airsr_stats_type

   TYPE stats_airsr_type
      TYPE (maxmin_airsr_stats_type)  :: maximum, minimum
      TYPE (residual_airsr1_type)     :: average, rms_err
   END TYPE stats_airsr_type

CONTAINS

#include "da_ao_stats_airsr.inc"
#include "da_calculate_jo_and_grady_airsr.inc"
#include "da_compute_jo_airsr_tq.inc"
#include "da_calculate_residual_airsr.inc"
#include "da_oi_stats_airsr.inc"
#include "da_print_stats_airsr.inc"
#include "da_transform_xtoy_airsr.inc"
#include "da_transform_xtoy_airsr_adj.inc"
#include "da_check_max_iv_airsr.inc"
#include "da_get_innov_vector_airsr.inc"
#include "da_calculate_grady_airsr.inc"

END MODULE da_airsr

