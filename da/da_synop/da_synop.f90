MODULE da_synop

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_physics
   USE par_util

   ! The "stats_synop_type" is ONLY used locally in DA_Synop:

   TYPE residual_synop1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_synop1_type

   TYPE maxmin_synop_stats_type
      TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_synop_stats_type

   TYPE stats_synop_type
      TYPE (maxmin_synop_stats_type)  :: maximum, minimum
      TYPE (residual_synop1_type)     :: average, rms_err
  END TYPE stats_synop_type

CONTAINS

#include "da_ao_stats_synop.inc"
#include "da_calculate_jo_and_grady_synop.inc"
#include "compute_jo_synop_uvtq.inc"
#include "da_calculate_residual_synop.inc"
#include "da_oi_stats_synop.inc"
#include "da_print_stats_synop.inc"
#include "da_transform_xtoy_synop.inc"
#include "da_transform_xtoy_synop_adj.inc"
#include "da_get_innov_vector_synop.inc"
#include "da_check_max_iv_synop.inc"
#include "da_calculate_grady_synop.inc"

END MODULE da_synop

