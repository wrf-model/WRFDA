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

         INCLUDE 'daxpy.inc'
         INCLUDE 'dcopy.inc'
         INCLUDE 'ddot.inc'
         INCLUDE 'dgemm.inc'
         INCLUDE 'dgemv.inc'
         INCLUDE 'dger.inc'
         INCLUDE 'dnrm2.inc'
         INCLUDE 'dscal.inc'
         INCLUDE 'dswap.inc'
         INCLUDE 'dsymv.inc'
         INCLUDE 'dsyr2.inc'
         INCLUDE 'dsyr2k.inc'
         INCLUDE 'dtrmm.inc'
         INCLUDE 'dtrmv.inc'
         INCLUDE 'lsame.inc'
         INCLUDE 'xerbla.inc'

      END MODULE BLAS

