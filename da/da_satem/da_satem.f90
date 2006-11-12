module da_satem

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_physics
   use da_tools

   ! The "stats_satem_type" is ONLY used locally in da_satem:

   type residual_satem1_type
      real          :: thickness                ! Satem thickness
   end type residual_satem1_type

   type maxmin_satem_stats_type
      type (maxmin_type)         :: thickness
   end type maxmin_satem_stats_type

   type stats_satem_type
      type (maxmin_satem_stats_type)  :: maximum, minimum
      type (residual_satem1_type)     :: average, rms_err
   end type stats_satem_type


contains

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

