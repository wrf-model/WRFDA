module da_gen_be

   !---------------------------------------------------------------------------- 
   ! Purpose: Collection of routines required by gen_be BE stats calculation 
   ! code.
   !----------------------------------------------------------------------------

   use da_control, only : stdout,vertical_ip, t0,es_beta,es_alpha, &
      es_gamma,kappa,base_pres,rd_over_rv,rd_over_rv1,t_kelvin, gravity, &
      da_array_print, filename_len,vertical_ip_0
   use da_reporting, only : da_error, message
   use da_tools1, only : da_get_unit, da_free_unit

   implicit none

contains

#include "da_create_bins.inc"
#include "da_filter_regcoeffs.inc"
#include "da_get_field.inc"
#include "da_get_height.inc"
#include "da_get_trh.inc"
#include "da_print_be_stats_h_global.inc"
#include "da_print_be_stats_h_regional.inc"
#include "da_print_be_stats_p.inc"
#include "da_print_be_stats_v.inc"
#include "da_readwrite_be_stage2.inc"
#include "da_readwrite_be_stage3.inc"
#include "da_readwrite_be_stage4.inc"
#include "da_stage0_initialize.inc"

   ! Files from other modules:
#include "da_transform_vptovv.inc"
#include "da_eof_decomposition.inc"
#include "da_eof_decomposition_test.inc"
#include "da_perform_2drf.inc"
#include "da_recursive_filter_1d.inc"

end module da_gen_be

subroutine wrf_abort
   stop
end subroutine wrf_abort
