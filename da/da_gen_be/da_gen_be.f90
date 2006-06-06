module da_gen_be

!------------------------------------------------------------------------------
!  PURPOSE: Collection of routines required by gen_be BE stats calculation code.
!
!  METHOD:  Varied.
!
!  HISTORY: 11/21/2004 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   use da_constants
   use lapack
   use blas
   use module_wrf_error
   use da_tracing

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

!Files from other modules:
#include "da_transform_vptovv.inc"
#include "da_eof_decomposition.inc"
#include "da_eof_decomposition_test.inc"
#include "da_perform_2drf.inc"
#include "da_recursive_filter_1d.inc"

end module da_gen_be

SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort
