module da_synop

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics

   ! The "stats_synop_type" is ONLY used locally in da_synop:

   type residual_synop1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_synop1_type

   type maxmin_synop_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_synop_stats_type

   type stats_synop_type
      type (maxmin_synop_stats_type)  :: maximum, minimum
      type (residual_synop1_type)     :: average, rms_err
   end type stats_synop_type

contains

#include "da_ao_stats_synop.inc"
#include "da_jo_and_grady_synop.inc"
#include "da_jo_synop_uvtq.inc"
#include "da_residual_synop.inc"
#include "da_oi_stats_synop.inc"
#include "da_print_stats_synop.inc"
#include "da_transform_xtoy_synop.inc"
#include "da_transform_xtoy_synop_adj.inc"
#include "da_get_innov_vector_synop.inc"
#include "da_check_max_iv_synop.inc"
#include "da_calculate_grady_synop.inc"

end module da_synop

