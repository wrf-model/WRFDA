module da_sound

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_physics

   ! The "stats_sound_type" is ONLY used locally in da_sound:

   type residual_sound1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_sound1_type

   type maxmin_sound_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_sound_stats_type

   type stats_sound_type
      type (maxmin_sound_stats_type)  :: maximum, minimum
      type (residual_sound1_type)     :: average, rms_err
   end type stats_sound_type

   ! The "stats_sonde_sfc_type" is ONLY used locally in da_sonde_sfc:

   type residual_sonde_sfc1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_sonde_sfc1_type

   type maxmin_sonde_sfc_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_sonde_sfc_stats_type

   type stats_sonde_sfc_type
      type (maxmin_sonde_sfc_stats_type)  :: maximum, minimum
      type (residual_sonde_sfc1_type)     :: average, rms_err
   end type stats_sonde_sfc_type

contains

#include "da_ao_stats_sound.inc"
#include "da_jo_and_grady_sound.inc"
#include "da_jo_sound_uvtq.inc"
#include "da_residual_sound.inc"
#include "da_oi_stats_sound.inc"
#include "da_print_stats_sound.inc"
#include "da_transform_xtoy_sound.inc"
#include "da_transform_xtoy_sound_adj.inc"
#include "da_check_max_iv_sound.inc"
#include "da_get_innov_vector_sound.inc"
#include "da_obs_diagnostics.inc"
#include "da_calculate_grady_sound.inc"

#include "da_ao_stats_sonde_sfc.inc"
#include "da_jo_and_grady_sonde_sfc.inc"
#include "da_jo_sonde_sfc_uvtq.inc"
#include "da_residual_sonde_sfc.inc"
#include "da_oi_stats_sonde_sfc.inc"
#include "da_print_stats_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc_adj.inc"
#include "da_get_innov_vector_sonde_sfc.inc"
#include "da_check_max_iv_sonde_sfc.inc"
#include "da_calculate_grady_sonde_sfc.inc"

end module da_sound

