module da_qscat

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_qscat_type" is ONLY used locally in da_qscat:

   type residual_qscat1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_qscat1_type

   type maxmin_qscat_stats_type
      type (maxmin_type)         :: u, v
   end type maxmin_qscat_stats_type

   type stats_qscat_type
      type (maxmin_qscat_stats_type)  :: maximum, minimum
      type (residual_qscat1_type)     :: average, rms_err
   end type stats_qscat_type

contains

#include "da_calculate_jo_and_grady_qscat.inc"
#include "da_calculate_residual_qscat.inc"
#include "da_check_max_iv_qscat.inc"
#include "da_get_innov_vector_qscat.inc"
#include "da_ao_stats_qscat.inc"
#include "da_oi_stats_qscat.inc"
#include "da_print_stats_qscat.inc"
#include "da_transform_xtoy_qscat.inc"
#include "da_transform_xtoy_qscat_adj.inc"
#include "da_calculate_grady_qscat.inc"

end module da_qscat
