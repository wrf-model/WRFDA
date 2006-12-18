      MODULE LAPACK

!------------------------------------------------------------------------------
!     PURPOSE: Contains driver routines downloaded from the LAPACL (linear 
!     algegra package) web site at http://www.netlib.org/lapack/
!
!     METHOD:  See individual routines which contain dependencies.
!
!     HISTORY: 01/07/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

      USE BLAS

!     .. Save statement ..
!     SAVE               FIRST, IWARN, LBETA, LEMAX, LEMIN, LEPS, LRMAX,
!    $                   LRMIN, LT

      LOGICAL :: FIRST, IWARN, LIEEE1, LRND
      INTEGER :: LBETA, LEMAX, LEMIN, LT
      REAL    :: LEPS, LRMAX, LRMIN, EMAX, EMIN, PREC, RMAX, RMIN,
     $           RND, SFMIN, BASE, T, SAFMX2, SAFMIN, SAFMN2

!      IMPLICIT NONE

      CONTAINS

         INCLUDE 'dlae2.inc'
         INCLUDE 'dlaev2.inc'
         INCLUDE 'dlamc1.inc'
         INCLUDE 'dlamc2.inc'
         INCLUDE 'dlamc3.inc'
         INCLUDE 'dlamc4.inc'
         INCLUDE 'dlamc5.inc'
         INCLUDE 'dlamch.inc'
         INCLUDE 'dlanst.inc'
         INCLUDE 'dlansy.inc'
         INCLUDE 'dlapy2.inc'
         INCLUDE 'dlarf.inc'
         INCLUDE 'dlarfb.inc'
         INCLUDE 'dlarfg.inc'
         INCLUDE 'dlarft.inc'
         INCLUDE 'dlartg.inc'
         INCLUDE 'dlascl.inc'
         INCLUDE 'dlaset.inc'
         INCLUDE 'dlasr.inc'
         INCLUDE 'dlasrt.inc'
         INCLUDE 'dlassq.inc'
         INCLUDE 'dlatrd.inc'
         INCLUDE 'dorg2l.inc'
         INCLUDE 'dorg2r.inc'
         INCLUDE 'dorgql.inc'
         INCLUDE 'dorgqr.inc'
         INCLUDE 'dorgtr.inc'
         INCLUDE 'dsteqr.inc'
         INCLUDE 'dsterf.inc'
         INCLUDE 'dsyev.inc'
         INCLUDE 'dsytd2.inc'
         INCLUDE 'dsytrd.inc'
         INCLUDE 'ieeeck.inc'
         INCLUDE 'ilaenv.inc'

      END MODULE LAPACK
