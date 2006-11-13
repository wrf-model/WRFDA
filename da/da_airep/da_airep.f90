module da_airep

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_airep_type" is ONLY used locally in da_airep:

   type residual_airep1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
   end type residual_airep1_type

   type maxmin_airep_stats_type
      type (maxmin_type)         :: u, v, t 
   end type maxmin_airep_stats_type

   type stats_airep_type
      type (maxmin_airep_stats_type)  :: maximum, minimum
      type (residual_airep1_type)     :: average, rms_err
   end type stats_airep_type

contains

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

