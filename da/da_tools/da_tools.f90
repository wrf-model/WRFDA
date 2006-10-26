MODULE da_tools
   
   !---------------------------------------------------------------------------
   ! PURPOSE: Contains general tools.
   !---------------------------------------------------------------------------
   
   USE module_bc

   USE da_control
   USE da_define_structures
   USE lapack
   use da_reporting
   
   IMPLICIT NONE
   
   ! Code copied from SI, see header below
#include "da_map_utils_defines.inc"

CONTAINS

#include "da_map_utils.inc"
#include "da_1d_eigendecomposition.inc"
#include "da_obs_sfc_correction.inc"
#include "da_sfcprs.inc"
#include "da_intpsfc_prs.inc"
#include "da_intpsfc_tem.inc"
#include "da_mo_correction.inc"
#include "da_diff_seconds.inc"
#include "da_global_ll_to_xy.inc"
#include "da_ll_to_xy.inc"
#include "da_residual.inc"
#include "da_add_noise.inc"
#include "da_max_error_qc.inc"
#include "da_random_omb.inc"
#include "da_random_seed.inc"
#include "da_set_randomcv.inc"
#include "da_gaus_noise.inc"
#include "da_llxy.inc"
#include "da_openfile.inc"
#include "da_smooth_anl.inc"
#include "da_togrid.inc"
#include "da_unifva.inc"
#include "da_xyll.inc"
#include "da_oi.inc"
#include "da_get_unit.inc"
#include "da_free_unit.inc"

#include "da_eof_decomposition_test.inc"
#include "da_eof_decomposition.inc"
#include "da_lubksb.inc"
#include "da_ludcmp.inc"
#include "da_set_boundary_xa.inc"
#include "da_set_boundary_xb.inc"
#include "da_set_boundary_3d.inc"

#include "da_get_2d_sum.inc"
#include "da_get_3d_sum.inc"
   
END MODULE da_tools

