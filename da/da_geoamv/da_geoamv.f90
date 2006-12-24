module da_geoamv

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

   ! The "stats_geoamv_type" is ONLY used locally in da_geoamv:

   type residual_geoamv1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_geoamv1_type

   type maxmin_geoamv_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_geoamv_stats_type

   type stats_geoamv_type
      type (maxmin_geoamv_stats_type)  :: maximum, minimum
      type (residual_geoamv1_type)     :: average, rms_err
   end type stats_geoamv_type

contains

#include "da_ao_stats_geoamv.inc"
#include "da_jo_and_grady_geoamv.inc"
#include "da_residual_geoamv.inc"
#include "da_oi_stats_geoamv.inc"
#include "da_print_stats_geoamv.inc"
#include "da_transform_xtoy_geoamv.inc"
#include "da_transform_xtoy_geoamv_adj.inc"
#include "da_check_max_iv_geoamv.inc"
#include "da_get_innov_vector_geoamv.inc"
#include "da_calculate_grady_geoamv.inc"

end module da_geoamv     

