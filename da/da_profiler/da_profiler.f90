module da_profiler

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_profiler_type" is ONLY used locally in da_profiler:

   type residual_profiler1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_profiler1_type

   type maxmin_profiler_stats_type
      type (maxmin_type)         :: u, v
   end type maxmin_profiler_stats_type

   type stats_profiler_type
      type (maxmin_profiler_stats_type)  :: maximum, minimum
      type (residual_profiler1_type)     :: average, rms_err
   end type stats_profiler_type

contains

#include "da_ao_stats_profiler.inc"
#include "da_jo_and_grady_profiler.inc"
#include "da_residual_profiler.inc"
#include "da_oi_stats_profiler.inc"
#include "da_print_stats_profiler.inc"
#include "da_transform_xtoy_profiler.inc"
#include "da_transform_xtoy_profiler_adj.inc"
#include "da_check_max_iv_profiler.inc"
#include "da_get_innov_vector_profiler.inc"
#include "da_calculate_grady_profiler.inc"


end module da_profiler

