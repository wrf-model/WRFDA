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

#include "dlae2.inc"
#include "dlaev2.inc"
#include "dlamc1.inc"
#include "dlamc2.inc"
#include "dlamc3.inc"
#include "dlamc4.inc"
#include "dlamc5.inc"
#include "dlamch.inc"
#include "dlanst.inc"
#include "dlansy.inc"
#include "dlapy2.inc"
#include "dlarf.inc"
#include "dlarfb.inc"
#include "dlarfg.inc"
#include "dlarft.inc"
#include "dlartg.inc"
#include "dlascl.inc"
#include "dlaset.inc"
#include "dlasr.inc"
#include "dlasrt.inc"
#include "dlassq.inc"
#include "dlatrd.inc"
#include "dorg2l.inc"
#include "dorg2r.inc"
#include "dorgql.inc"
#include "dorgqr.inc"
#include "dorgtr.inc"
#include "dsteqr.inc"
#include "dsterf.inc"
#include "dsyev.inc"
#include "dsytd2.inc"
#include "dsytrd.inc"
#include "ieeeck.inc"
#include "ilaenv.inc"

      END MODULE LAPACK
