module da_gen_be

!------------------------------------------------------------------------------
!  PURPOSE: Collection of routines required by gen_be BE stats calculation code.
!
!  METHOD:  Varied.
!
!  HISTORY: 11/21/2004 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   use da_constants
   use LAPACK
   use BLAS

   implicit none

contains

#include "da_create_bins.inc"
#include "da_readwrite_be_stage2.inc"
#include "da_readwrite_be_stage3.inc"
#include "da_readwrite_be_stage4.inc"

!Files from other modules:
#include "../DA_VToX_Transforms/DA_Transform_VpToVv.inc"
#include "../DA_Tools/da_eof_decomposition.inc"
#include "../DA_Tools/da_eof_decomposition_test.inc"

end module da_gen_be
