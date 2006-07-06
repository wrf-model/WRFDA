MODULE da_pseudo

   use da_constants
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util

! The "stats_pseudo_type" is ONLY used locally in DA_Pseudo:

   TYPE residual_pseudo1_type
      REAL          :: u                        ! u-wind.
      REAL          :: v                        ! v-wind.
      REAL          :: t                        ! temperature
      REAL          :: p                        ! pressure
      REAL          :: q                        ! specific humidity
   END TYPE residual_pseudo1_type

   TYPE maxmin_pseudo_stats_type
        TYPE (maxmin_type)         :: u, v, t, p, q
   END TYPE maxmin_pseudo_stats_type

   TYPE stats_pseudo_type
        TYPE (maxmin_pseudo_stats_type)  :: maximum, minimum
        TYPE (residual_pseudo1_type)     :: average, rms_err
   END TYPE stats_pseudo_type

CONTAINS

#include "da_cal_jo_and_grady_pseudo.inc"
#include "da_calculate_residual_pseudo.inc"
#include "da_get_innov_vector_pseudo.inc"
#include "da_ao_stats_pseudo.inc"
#include "da_oi_stats_pseudo.inc"
#include "da_print_stats_pseudo.inc"
#include "da_transform_xtoy_pseudo.inc"
#include "da_transform_xtoy_pseudo_adj.inc"
#include "da_calculate_grady_pseudo.inc"

END MODULE da_pseudo

