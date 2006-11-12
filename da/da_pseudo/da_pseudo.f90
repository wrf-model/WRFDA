module da_pseudo

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

! The "stats_pseudo_type" is ONLY used locally in da_pseudo:

   type residual_pseudo1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_pseudo1_type

   type maxmin_pseudo_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_pseudo_stats_type

   type stats_pseudo_type
      type (maxmin_pseudo_stats_type)  :: maximum, minimum
      type (residual_pseudo1_type)     :: average, rms_err
   end type stats_pseudo_type

contains

#include "da_cal_jo_and_grady_pseudo.inc"
#include "da_calculate_residual_pseudo.inc"
#include "da_get_innov_vector_pseudo.inc"
#include "da_ao_stats_pseudo.inc"
#include "da_oi_stats_pseudo.inc"
#include "da_print_stats_pseudo.inc"
#include "da_transform_xtoy_pseudo.inc"
#include "da_transform_xtoy_pseudo_adj.inc"
#include "da_calculate_grady_pseudo.inc"

end module da_pseudo

