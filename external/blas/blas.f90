      MODULE BLAS

!------------------------------------------------------------------------------
!     PURPOSE: This module CONTAINS routines within the BLAS (basic linear 
!     algegra software) package that are currently used within the WRFVAR system.
!
!     Routines not currently used are contained in the Not_Yet_Used subdir. They
!     will be added to this module as and when required.
!
!     The BLAS site is at http://www.netlib.org/blas
!
!     METHOD: Various basic linear algebra routines used for example in LAPACK.
!             These routines are not optimised for any particular machine.
!
!     HISTORY: 01/07/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

!      IMPLICIT NONE

      CONTAINS

#include "daxpy.inc"
#include "dcopy.inc"
#include "ddot.inc"
#include "dgemm.inc"
#include "dgemv.inc"
#include "dger.inc"
#include "dnrm2.inc"
#include "dscal.inc"
#include "dswap.inc"
#include "dsymv.inc"
#include "dsyr2.inc"
#include "dsyr2k.inc"
#include "dtrmm.inc"
#include "dtrmv.inc"
#include "lsame.inc"
#include "xerbla.inc"

      END MODULE BLAS

