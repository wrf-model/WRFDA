MODULE da_sound

   USE da_constants
   USE da_define_structures
   USE da_interpolation
   USE da_statistics
   USE da_tools
   USE da_physics
   USE par_util

   ! The "stats_sound_type" is ONLY used locally in DA_Sound:

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

   ! The "stats_sonde_sfc_type" is ONLY used locally in DA_Sonde_Sfc:

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

#include "DA_AO_Stats_Sound.inc"
#include "DA_Calculate_Jo_and_GradY_Sound.inc"
#include "compute_jo_sound_uvtq.inc"
#include "DA_Calculate_Residual_Sound.inc"
#include "DA_OI_Stats_Sound.inc"
#include "DA_Print_Stats_Sound.inc"
#include "DA_Transform_XToY_Sound.inc"
#include "DA_Transform_XToY_Sound_Adj.inc"
#include "da_check_max_iv_sound.inc"
#include "da_get_innov_vector_sound.inc"
#include "da_obs_diagnostics.inc"
#include "DA_Calculate_GradY_Sound.inc"

#include "DA_AO_Stats_Sonde_sfc.inc"
#include "DA_Get_Jo_and_GradY_Sonde_sfc.inc"
#include "compute_jo_sonde_sfc_uvtq.inc"
#include "DA_Calculate_Residual_Sonde_sfc.inc"
#include "DA_OI_Stats_Sonde_sfc.inc"
#include "DA_Print_Stats_Sonde_sfc.inc"
#include "DA_Transform_XToY_Sonde_sfc.inc"
#include "DA_Transform_XToY_Sonde_sfc_Adj.inc"
#include "da_get_innov_vector_sonde_sfc.inc"
#include "da_check_max_iv_sonde_sfc.inc"
#include "DA_Calculate_GradY_Sonde_sfc.inc"

END MODULE da_sound

