module da_profiler

   use da_constants
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_profiler_type" is ONLY used locally in da_profiler:

   TYPE residual_profiler1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
   END TYPE residual_profiler1_type

   TYPE maxmin_profiler_stats_type
      TYPE (maxmin_type)         :: u, v
   END TYPE maxmin_profiler_stats_type

   TYPE stats_profiler_type
      TYPE (maxmin_profiler_stats_type)  :: maximum, minimum
      TYPE (residual_profiler1_type)     :: average, rms_err
   END TYPE stats_profiler_type

CONTAINS

#include "da_ao_stats_profiler.inc"
#include "da_get_jo_and_grady_profiler.inc"
#include "da_calculate_residual_profiler.inc"
#include "da_oi_stats_profiler.inc"
#include "da_print_stats_profiler.inc"
#include "da_transform_xtoy_profiler.inc"
#include "da_transform_xtoy_profiler_adj.inc"
#include "da_check_max_iv_profiler.inc"
#include "da_get_innov_vector_profiler.inc"
#include "da_calculate_grady_profiler.inc"


end module da_profiler

