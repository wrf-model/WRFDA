module da_bogus

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics

   ! The "stats_bogus_type" is ONLY used locally in da_bogus:

   type residual_bogus1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
      real          :: slp                      ! sea level pressure (hPa)
   end type residual_bogus1_type

   type maxmin_bogus_stats_type
      type (maxmin_type)         :: u, v, t, q, slp 
   end type maxmin_bogus_stats_type

   type stats_bogus_type
      type (maxmin_bogus_stats_type)  :: maximum, minimum
      type (residual_bogus1_type)     :: average, rms_err
   end type stats_bogus_type

contains

#include "da_ao_stats_bogus.inc"
#include "da_jo_and_grady_bogus.inc"
#include "da_residual_bogus.inc"
#include "da_oi_stats_bogus.inc"
#include "da_print_stats_bogus.inc"
#include "da_transform_xtoy_bogus.inc"
#include "da_transform_xtoy_bogus_adj.inc"
#include "da_check_max_iv_bogus.inc"
#include "da_get_innov_vector_bogus.inc"
#include "da_calculate_grady_bogus.inc"

end module da_bogus

