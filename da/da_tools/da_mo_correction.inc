FUNCTION da_mo_correction (ho, po, to, qo, uo, vo, &
                        hm, pm, tm, qm, um ,vm, &
                        roughness)   RESULT (correc)
!------------------------------------------------------------------------------!
! Compute the correction factor to convert surface wind into 40m wind
! using similarity laws.
!
! Reference:
! ---------
!  A description of the fifth generation Penn State/NCAR Mesoscale Model (MM5)
!  Grell et al. 1994, page 29-30 and 80-83.
!  
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      REAL, INTENT (in)                :: ho, po, to, qo, uo, vo
      REAL, INTENT (in)                :: hm, pm, tm, qm, um, vm
      REAL, INTENT (in)                :: roughness

      REAL                             :: correc, winfac

      REAL :: thm , tho, tvm, tvo, thvm, thvo, rcp
      REAL :: za, Vc2, Va2, V2 
      REAL :: hdif, rib, rll, hll, zint
      LOGICAL :: correction

      !  Height difference (in m) above wich correction is applied

      REAL, PARAMETER :: hmax = 10., hs_max = 40.   

      !  Default Roughness length im m (for land, set to 0 if on sea)

      REAL, PARAMETER :: zint0 = 0.2

!------------------------------------------------------------------------------!
      rcp = gas_constant/cp

! 0.  INITIALIZE CORRECTION FACTOR TO 1
! =====================================

      correc = 1.
      winfac = 1.

! 1.  HEIGHT DIFFERENCE AND ROUGHNESS LENGTH
! ==========================================

! 1.1 Height difference
!     -----------------

      hdif = hm - ho

! 1.2 No correction if height difference is less than hmax. 
!     -----------------------------------------------------

      IF  (hdif <= hmax) THEN

           RETURN

      ENDIF

! 1.3 Compute the roughness length based upon season and land use 
!     -----------------------------------------------------------

      zint = roughness

      if( zint < 0.0001) zint = 0.0001

! 2.  POTENTIAL TEMPERATURE AT MODEL SURFACE AND OBSERVATION LOCATION
! ===================================================================

! 2.1 Potential temperature on model surface
!     --------------------------------------

      thm  = tm * (1000. / (pm/100.)) ** rcp

! 2.2 Potential temperature at observation location
!     ---------------------------------------------

      tho  = to * (1000. / (po/100.)) ** rcp


! 3.  VIRTUAL TEMPERATURE AT MODEL SURFACE AND OBSERVATION LOCATION
! ===================================================================

! 3.1 Virtual temperature on model surface
!     -------------------------------------

      tvm  = tm * (1. + 0.608 * qm)

! 3.2 Virtual temperature at observation location
!     -------------------------------------------

      tvo  = to * (1. + 0.608 * qo)


! 4.  POTENTIAL VIRTUAL TEMPERATURE AT MODEL SURFACE AND OBSERVATION LOCATION 
! ===========================================================================

! 4.1 Potential virtual temperature on model surface
!     ----------------------------------------------

      thvm = tvm * (1000. / (pm/100.)) ** rcp

! 4.2 potential Virtual temperature at observation location
!     -----------------------------------------------------

      thvo = tvo * (1000. / (po/100.)) ** rcp


! 5.  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH
! =================================================

! 5.1 Pre-calculations
!     ----------------

      za  = hm - ho

!      Because the following formula is derived based on
!      the surface layer height is hs_max=40.m. Under
!      free convection, we assume that atmospheric state
!      above the surface layer is fully mixed, and the
!      wind at the lowest sigma half level is same as the
!      wind at top of the surface layer. 

      if (za > hs_max) za = hs_max
      
      Va2 =   um*um + vm*vm
      Vc2 = 4. * MAX ((tho - thm), 0.)

      V2  = Va2 + Vc2

! 5.2 Bulk richardson number
!     ----------------------

      rib = (gravity * za / thm) * (thvm - thvo) / V2

! 5.3 Monin-obukov length
!     -------------------

      hll = rib * LOG (za/zint)

      rll = za / hll

! 5.4 Ratio PBL height/Monin Obukov length: za/rll
!     ------------------------------------

      hll =  ABS (hll)


! 6.  CORRECTION FACTOR BASED UPON REGIME
! =======================================

! 6.1 Stable conditions (REGIME 1)
!     ---------------------------
 
!     correc = 1.      !  rib > 0.2

! 6.2 Mechanically driven turbulence (REGIME 2)
!     ------------------------------------------

!     correc = 1.      !  0.0 =< rib <= 0.2


      correc = 1.

      IF (rib < 0.) THEN

! 6.3 Unstable Forced convection (REGIME 3)
!     -------------------------------------

!        correc = 1.  !   hll <= 1.5


! 6.4 Free convection (REGIME 4)
!     --------------------------

         IF(hll > 1.5) THEN

           IF (zint < 0.2) THEN
               correc = 1.000 + 0.320 * zint ** 0.200
           ELSE IF (zint < 0.) THEN
               correc = 1.169 + 0.315 * zint
           ENDIF

! 6.4.1 The factor depended on Za (MM5, param.F)
      
           winfac = 0.5*(log(za/0.05)/log(40./0.05) &
                       + log(za)/log(40.))

           correc =  correc * winfac
   
         ENDIF

      ENDIF

END FUNCTION da_mo_correction
