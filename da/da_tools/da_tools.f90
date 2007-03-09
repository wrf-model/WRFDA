module da_tools
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains general tools.
   !---------------------------------------------------------------------------
   
   use module_bc, only : bdyzone
   use module_dm, only : wrf_dm_sum_real
   use module_domain, only : xb_type, xpose_type

#ifndef crayx1
   use lapack, only : dsyev
#endif

   use da_control, only : pi, gravity, gas_constant, ims, ime, jms,jme, &
      kms,kme,its,ite,jts,jte,kts,kte,ids,ide,stdout, &
      trace_use, da_array_print, fg_format_kma_global, coarse_ds, coarse_ix, &
      coarse_jy, fg_format, c2, cone_factor, earth_radius, dsm, &
      map_projection, psi1, pole, start_x, phic, start_y, xlonc, ycntr, &
      put_rand_seed, seed_array1, seed_array2, obs_qc_pointer, &
      set_omb_rand_fac, fails_error_max, missing_r,x_start_sub_domain, global, &
      fg_format_wrf, x_end_sub_domain, y_end_sub_domain, def_sub_domain, &
      y_start_sub_domain, start_lat, delt_lat, delt_lon, start_lon, cp, &
      missing, surface_correction,print_detail_map, oi_use, use_rad, stderr
   use da_define_structures, only : info_type, field_type, x_type,  &
      model_loc_type, synop_type, bad_info_type, da_gauss_noise, &
      ob_type, y_type
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_reporting, only : da_error, message, da_warning
   
   implicit none
   
   ! Code copied from SI, see header below
#include "da_map_utils_defines.inc"

contains

#include "da_ijll_lc.inc"
#include "da_llij_lc.inc"
#include "da_set_lc.inc"
#include "da_ijll_ps.inc"
#include "da_llij_ps.inc"
#include "da_set_ps.inc"
#include "da_map_init.inc"
#include "da_llij_latlon.inc"
#include "da_ijll_latlon.inc"
#include "da_latlon_to_ij.inc"
#include "da_ij_to_latlon.inc"
#include "da_map_set.inc"
#include "da_ijll_merc.inc"
#include "da_llij_merc.inc"
#include "da_set_merc.inc"
#include "da_lc_cone.inc"

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
#include "da_residual_new.inc"
#include "da_add_noise.inc"
#include "da_add_noise_new.inc"
#include "da_max_error_qc.inc"
#include "da_random_omb.inc"
#include "da_random_seed.inc"
#include "da_set_randomcv.inc"
#include "da_gaus_noise.inc"
#include "da_llxy.inc"
#include "da_openfile.inc"
#include "da_smooth_anl.inc"
#include "da_togrid_new.inc"
#include "da_togrid.inc"
#include "da_unifva.inc"
#include "da_xyll.inc"

#include "da_eof_decomposition_test.inc"
#include "da_eof_decomposition.inc"
#include "da_lubksb.inc"
#include "da_ludcmp.inc"
#include "da_set_boundary_xa.inc"
#include "da_set_boundary_xb.inc"
#include "da_set_boundary_3d.inc"

#include "da_get_2d_sum.inc"
#include "da_get_3d_sum.inc"

#include "da_oi.inc"
   
end module da_tools

