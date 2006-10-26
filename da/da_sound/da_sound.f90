MODULE da_sound

   USE da_control
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_physics

   ! The "stats_sound_type" is ONLY used locally in da_sound:

   TYPE residual_sound1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: q                        ! specific humidity
   END TYPE residual_sound1_type

   TYPE maxmin_sound_stats_type
      TYPE (maxmin_type)         :: u, v, t, q
   END TYPE maxmin_sound_stats_type

   TYPE stats_sound_type
      TYPE (maxmin_sound_stats_type)  :: maximum, minimum
      TYPE (residual_sound1_type)     :: average, rms_err
   END TYPE stats_sound_type

   ! The "stats_sonde_sfc_type" is ONLY used locally in da_sonde_sfc:

   TYPE residual_sonde_sfc1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_sonde_sfc1_type

   TYPE maxmin_sonde_sfc_stats_type
      TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_sonde_sfc_stats_type

   TYPE stats_sonde_sfc_type
      TYPE (maxmin_sonde_sfc_stats_type)  :: maximum, minimum
      TYPE (residual_sonde_sfc1_type)     :: average, rms_err
   END TYPE stats_sonde_sfc_type

CONTAINS

#include "da_ao_stats_sound.inc"
#include "da_calculate_jo_and_grady_sound.inc"
#include "da_compute_jo_sound_uvtq.inc"
#include "da_calculate_residual_sound.inc"
#include "da_oi_stats_sound.inc"
#include "da_print_stats_sound.inc"
#include "da_transform_xtoy_sound.inc"
#include "da_transform_xtoy_sound_adj.inc"
#include "da_check_max_iv_sound.inc"
#include "da_get_innov_vector_sound.inc"
#include "da_obs_diagnostics.inc"
#include "da_calculate_grady_sound.inc"

#include "da_ao_stats_sonde_sfc.inc"
#include "da_get_jo_and_grady_sonde_sfc.inc"
#include "da_compute_jo_sonde_sfc_uvtq.inc"
#include "da_calculate_residual_sonde_sfc.inc"
#include "da_oi_stats_sonde_sfc.inc"
#include "da_print_stats_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc_adj.inc"
#include "da_get_innov_vector_sonde_sfc.inc"
#include "da_check_max_iv_sonde_sfc.inc"
#include "da_calculate_grady_sonde_sfc.inc"

END MODULE da_sound

