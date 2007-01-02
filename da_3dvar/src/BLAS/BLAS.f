      MODULE BLAS

!------------------------------------------------------------------------------
!     PURPOSE: This module CONTAINS routines within the BLAS (basic linear 
!     algegra software) package that are currently used within the 3DVAR system.
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

         INCLUDE 'daxpy.f'
         INCLUDE 'dcopy.f'
         INCLUDE 'ddot.f'
         INCLUDE 'dgemm.f'
         INCLUDE 'dgemv.f'
         INCLUDE 'dger.f'
         INCLUDE 'dnrm2.f'
         INCLUDE 'dscal.f'
         INCLUDE 'dswap.f'
         INCLUDE 'dsymv.f'
         INCLUDE 'dsyr2.f'
         INCLUDE 'dsyr2k.f'
         INCLUDE 'dtrmm.f'
         INCLUDE 'dtrmv.f'
         INCLUDE 'lsame.inc'
         INCLUDE 'xerbla.inc'

      END MODULE BLAS

