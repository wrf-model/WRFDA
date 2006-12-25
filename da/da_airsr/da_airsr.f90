module da_airsr

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics
   use da_par_util1
   use da_par_util

   ! The "stats_airsr_type" is ONLY used locally in da_airsr:

   type residual_airsr1_type
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_airsr1_type

   type maxmin_airsr_stats_type
      type (maxmin_type)         :: t, q
   end type maxmin_airsr_stats_type

   type stats_airsr_type
      type (maxmin_airsr_stats_type)  :: maximum, minimum
      type (residual_airsr1_type)     :: average, rms_err
   end type stats_airsr_type

contains

#include "da_ao_stats_airsr.inc"
#include "da_jo_and_grady_airsr.inc"
#include "da_jo_airsr_tq.inc"
#include "da_residual_airsr.inc"
#include "da_oi_stats_airsr.inc"
#include "da_print_stats_airsr.inc"
#include "da_transform_xtoy_airsr.inc"
#include "da_transform_xtoy_airsr_adj.inc"
#include "da_check_max_iv_airsr.inc"
#include "da_get_innov_vector_airsr.inc"
#include "da_calculate_grady_airsr.inc"

end module da_airsr

