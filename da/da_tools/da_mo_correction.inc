SUBROUTINE da_intpsfc_tem (val, ho, po, to, height, pressure, temp, kts, kte)
!------------------------------------------------------------------------------!
!
! Correct temperature between two levels.
!
! Reference: 
! ---------
! The use of surface observations in four dimensional data assimilation
!  Using a mesoscale model.
!  Ruggiero et al., 1996, Monthly Weather Review, Volume 124, 1018-1033
!
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      REAL,    INTENT (out):: val
      INTEGER, INTENT (in) :: kts, kte
      REAL,    INTENT (in) :: ho, po, to
      REAL, DIMENSION (kts:kte), INTENT (in) :: height, pressure, temp

      REAL, DIMENSION (kts:kte) :: prs_mb
      REAL    :: dth_12, dth_21, dth_sfc, dth_obs
      REAL    :: dhe_12, dhe_21, dhe_sfc1, dhe_obs1, dhe_sfc2, dhe_obs2
      REAL    :: th_100mb, th_200mb, th_obs, th_sfc
      REAL    :: th_obs_int, th_sfc_int
      REAL    :: pdif, rcp
      INTEGER :: k_100mb, k_200mb, jk
      INTEGER :: inc_100mb, inc_200mb

!------------------------------------------------------------------------------!
      rcp = gas_constant/cp

! 1.   FIND LEVELS: MODEL SURFACE + 100hPa AND MODEL SURFACE + 200hPa AR OBS LOC
! ==============================================================================

! 1.1 Convert model pressure profile from Pa to hPa  
!     ---------------------------------------------

       prs_mb = pressure / 100.

! 1.2  Find levels surface + 100hPA
!      ----------------------------

       inc_100mb = 100.
       k_100mb   = kts


       DO jk =  kts+1, kte
          pdif = prs_mb (kts) - prs_mb (jk)
          IF (pdif .GE. inc_100mb) THEN
              k_100mb = jk
              EXIT
          ENDIF
       ENDDO

! 1.2  Find levels surface + 200hPA
!      ----------------------------

       inc_200mb = 200.
       k_200mb   = kts

       DO jk =  kts+1, kte
          pdif = prs_mb (kts) - prs_mb (jk)
          IF (pdif .GE. inc_200mb) THEN
              k_200mb = jk
              EXIT
          ENDIF
       ENDDO

! 1.3  Check consistency 
!      -----------------

       IF ((k_100mb .LE. kts) .OR. (k_200mb .LE. kts) .OR. &
           (k_200mb .LT. k_100mb)) THEN
            WRITE (UNIT=errmsg(1),FMT='(A)') ' ERROR CANNOT FIND SFC + 200hPA AND SFC + 100hPA' 
            WRITE (UNIT=errmsg(2),FMT='(A,I2,A,F10.3)') ' P (',k_200mb,') = ',prs_mb (k_200mb) 
            WRITE (UNIT=errmsg(3),FMT='(A,I2,A,F10.3)') ' P (',k_100mb,') = ',prs_mb (k_100mb) 
            WRITE (UNIT=errmsg(4),FMT='(A,F10.3)')      ' P_SFC  = ',         prs_mb (kts) 
            CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1:4))
       ENDIF


! 2.  POTENTIAL TEMPERATURE 
! =========================

! 2.1 Potential temperature at 100hPa above model surface
!     ---------------------------------------------------

      th_100mb = temp (k_100mb) * (1000. / prs_mb (k_100mb))**rcp

! 2.2 Potential temperature at 200hPa above model surface
!     ---------------------------------------------------

      th_200mb = temp (K_200mb) * (1000. / prs_mb (k_200mb))**rcp

! 2.3 Potential temperature at observation location
!     ---------------------------------------------

      th_obs   = to * (1000. / (po/100.)) ** rcp


! 3.  LAPSE RATE BETWEEN SURFACE+200hPA AND SURFACE+100hPa
! =========================================================

! 3.1 Potential temperature increment
!     -------------------------------
    
      dth_21 = th_100mb - th_200mb
      dth_12 = th_200mb - th_100mb

! 3.1 Heigth increments
!     ----------------
    
      dhe_21   = height (k_100mb )- height (k_200mb)
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
