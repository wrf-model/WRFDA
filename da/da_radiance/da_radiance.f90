module da_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use da_radiance1

contains

#include "da_jo_and_grady_rad.inc"
#include "da_residual_rad.inc"
#include "da_biascorr_rad.inc"
#include "da_biasprep.inc"
#include "da_write_biasprep.inc"
#include "da_predictor.inc"
#include "da_qc_rad.inc"
#include "da_qc_amsua.inc"
#include "da_qc_amsub.inc"
#include "da_write_iv_rad_ascii.inc"
#include "da_write_oa_rad_ascii.inc"
#include "da_get_innov_vector_rad.inc"
#include "da_detsurtyp.inc"
#include "da_oma_stats_rad.inc"
#include "da_omb_stats_rad.inc"
#include "da_print_stats_rad.inc"
#include "da_transform_xtoy_rad.inc"
#include "da_transform_xtoy_rad_adj.inc"
#include "da_calculate_grady_rad.inc"

end module da_radiance

