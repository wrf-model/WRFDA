module da_gpspw

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_physics
   use da_tools

   ! The "stats_gpspw_type" is ONLY used locally in da_gpspw:

   type residual_gpspw1_type
      real          :: tpw                      ! Precipitable water
   end type residual_gpspw1_type

   type maxmin_gpspw_stats_type
      type (maxmin_type)         :: tpw
   end type maxmin_gpspw_stats_type

   type stats_gpspw_type
      type (maxmin_gpspw_stats_type)  :: maximum, minimum
      type (residual_gpspw1_type)     :: average, rms_err
   end type stats_gpspw_type

contains

#include "da_ao_stats_gpspw.inc"
#include "da_jo_and_grady_gpspw.inc"
#include "da_residual_gpspw.inc"
#include "da_oi_stats_gpspw.inc"
#include "da_print_stats_gpspw.inc"
#include "da_transform_xtoy_gpspw.inc"
#include "da_transform_xtoy_gpspw_adj.inc"
#include "da_check_max_iv_gpspw.inc"
#include "da_get_innov_vector_gpspw.inc"
#include "da_calculate_grady_gpspw.inc"


end module da_gpspw

