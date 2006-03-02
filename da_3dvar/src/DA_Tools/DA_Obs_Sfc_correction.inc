SUBROUTINE DA_Obs_Sfc_correction(sfc_obs, xb, sfc_assi_options)

!--------------------------------------------------------------------!
! This subroutine is used to correct the surface measurements (wind, !
! temperature, and pressure) from the observed height to the WRF     !
! model's lowest half-zeta level before going to the minimization.   !
!                                                                    !
!   Wind       : based on the similarity theory                      !
!   Temperature: Frank Ruggiero's (1996) method                      !
!   Pressure   : Hydrostatic correction                              !
!                                                                    !
! The order of the vertical index is "kts=1(bottom) and kte(top)".   !
! With cv_options=2 and sfc_assi_option=1, this procedure must be    !
! gone through, otherwise the unrealistic results may be obtained.   !
!                                                                    !
!                                       Yong-Run Guo 02/13/2004      !
!--------------------------------------------------------------------!
   IMPLICIT NONE

   TYPE(synop_type),       intent(inout) :: sfc_obs
   TYPE(xb_type),          intent(in)    :: xb
   integer,                intent(in)    :: sfc_assi_options

   REAL                                  :: roughness, psfc, &
                                            mslp, dx, dxm, dy, dym,    &
                                            ho, po, to, qo, uo, vo,   &
                                            hm, pm, tm, qm, um, vm, correc, val, &
                                            es, qs

   INTEGER                               :: i, j, k

   REAL, dimension(xb%kts:xb%kte)        :: t_mdl, q_mdl, u_mdl, v_mdl, &
                                            height, pressure

! -----------------------------------------------------------------------

! 1. Check if it needs to do the surface correction at the first level
 
!  1.1 Surface repots located at far below the lowest model level
 
! 2. Model profile at OBS site for surface correction

!     WRITE(0,'(A)') "MODEL PROFILE:"

         i = sfc_obs%loc%i
         j = sfc_obs%loc%j
         dx = sfc_obs%loc%dx
         dy = sfc_obs%loc%dy
         dxm = sfc_obs%loc%dxm
         dym = sfc_obs%loc%dym

!-----Model roughness at the obs site

      roughness = dym*(dxm*xb%rough(i,j)   + dx*xb%rough(i+1,j)) &
                + dy *(dxm*xb%rough(i,j+1) + dx*xb%rough(i+1,j+1))

      model_profile: do k = xb%kts, xb%kte

         pressure(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                     + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))

         height(k)   = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                     + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))

         t_mdl(k)    = dym*(dxm*xb%t(i,j  ,k) + dx*xb% t(i+1,j  ,k)) &
                     + dy *(dxm*xb%t(i,j+1,k) + dx*xb% t(i+1,j+1,k))

         q_mdl(k)    = dym*(dxm*xb%q(i,j  ,k) + dx*xb% q(i+1,j  ,k)) &
                     + dy *(dxm*xb%q(i,j+1,k) + dx*xb% q(i+1,j+1,k))

         u_mdl(k)    = dym*(dxm*xb%u(i,j  ,k) + dx*xb% u(i+1,j  ,k)) &
                     + dy *(dxm*xb%u(i,j+1,k) + dx*xb% u(i+1,j+1,k))

         v_mdl(k)    = dym*(dxm*xb%v(i,j  ,k) + dx*xb% v(i+1,j  ,k)) &
                     + dy *(dxm*xb%v(i,j+1,k) + dx*xb% v(i+1,j+1,k))

     enddo model_profile

! 3. OBS data and recover the surface pressure from the
!    mean sea level pressure (mslp)

      ho   = sfc_obs % h
      po   = sfc_obs % p % inv 
      to   = sfc_obs % t % inv
      qo   = sfc_obs % q % inv
      uo   = sfc_obs % u % inv
      vo   = sfc_obs % v % inv

! 3.1 Compute the surface OBS pressure from mean sea level pressure
 
      mslp = sfc_obs % loc % slp % inv
      if (abs(mslp - missing_r) > 1.) then
         psfc = missing_r
         if (abs(ho - missing_r) > 1.) then
            if (abs(to - missing_r) > 1.) then
               CALL sfcprs (xb%kts, xb%kte, pressure, t_mdl, q_mdl, &
                            height, psfc, mslp, ho, to)
            else
               CALL sfcprs (xb%kts, xb%kte, pressure, t_mdl, q_mdl, &
                            height, psfc, mslp, ho)
            endif
         endif
         sfc_obs % p % inv = psfc
! YRG: to allow assimlate the Psfc from mslp:
         sfc_obs % p % qc  = 0
      endif

      if(sfc_obs % p % inv < 1.0) then
         sfc_obs % p % qc  = missing
      endif

! If sfc_assi_options = 2, no further actions taken (Guo 03/01/2006):
      if (sfc_assi_options == 2) return

      po = sfc_obs % p % inv

! 3.2 Check that obs pressure and height are present
!     ----------------------------------------------

      IF (abs(po - missing_r) < 1.  .OR. &
          abs(ho - missing_r) < 1.) THEN

          return

           WRITE(unit=*, fmt='(/3(1x,a))') &
                'MISSING HEIGHT OR PRESSURE OBSERVATION ID ', &
                 TRIM (sfc_obs%info % id), TRIM (sfc_obs%info % name)

           WRITE(unit=*, fmt='(2(A,F12.3,/))') &
                               ' height   = ',ho,&
                               ' pressure = ',po
!          STOP                ' in DA_Sfc_correction'

      ENDIF

! 4.  BRING SURFACE OBSERVATION BELOW MODEL LEVELS with GOOD HEIGHT QUALITY 
! ================================================

      IF ( sfc_obs % h < height(xb%kts)) THEN

! 2.3 Make use of local variables for model
!     -------------------------------------

        um = u_mdl(xb%kts)
        vm = v_mdl(xb%kts)
        tm = t_mdl(xb%kts)
        pm = pressure (xb%kts)
        qm = q_mdl(xb%kts)
        hm = height(xb%kts)

! 3.2 Correction wind based on similarity laws
!     -------------------------------------------

           IF ((abs(sfc_obs % u % inv - missing_r) > 1.).AND.&
               (abs(sfc_obs % v % inv - missing_r) > 1.))THEN

! 3.2.1 Correction factor
 
!      temperature and moisture at sigma level:
       
               IF (abs(to - missing_r) < 1.0) then

                  correc = MO_CORRECTION( ho, po, tm, qo, uo, vo, &
                                          hm, pm, tm, qm, um ,vm, roughness )
               ELSE

                  correc = MO_CORRECTION( ho, po, to, qo, uo, vo, &
                                          hm, pm, tm, qm, um ,vm, roughness )

               ENDIF

! 3.2.2 Wind correction 
!       ---------------

               !  Correct data and replace any previous wind qcs
               !  with surface correction flag

               sfc_obs % u % inv = correc * sfc_obs % u % inv 
               sfc_obs % u % qc  = surface_correction

               sfc_obs % v % inv = correc * sfc_obs % v % inv
               sfc_obs % v % qc  = surface_correction

           ENDIF

! 3.4 Correct pressure
!     ----------------

     
           IF (sfc_obs % p % qc >= 0) THEN

             !  Correct data

             if (abs(to  - missing_r) > 1. .and. &
                 abs(qo - missing_r) > 1.) then
                 CALL intpsfc_prs (val, ho, po, hm, pm, tm, qm, to, qo)
             else if (abs(to  - missing_r) > 1.) then
                 CALL intpsfc_prs (val, ho, po, hm, pm, tm, qm, to)
             else
                 CALL intpsfc_prs (val, ho, po, hm, pm, tm, qm)
             endif

             !  Replace any previous pressure qc by the surface correction

               sfc_obs % p % inv = val
               sfc_obs % p % qc  = surface_correction

           ENDIF

! 3.5 Correct temperature
!     -------------------

           IF (abs(sfc_obs % t % inv - missing_r) > 1.) THEN

               !  Correct data

               CALL intpsfc_tem (val, ho, po, to, height, pressure, t_mdl, xb%kts, xb%kte)

               sfc_obs % t % inv = val

               !  Replace any previous temperature qc by the surface correction

               sfc_obs % t % qc  = surface_correction

           ENDIF

! 3.6 Assign model lowest level height + 1m to observation
!      ----------------------------------------------------- 

!         sfc_obs % h = height(xb%kts) + 1.

! 3.7 Height QC
!     ---------

!         sfc_obs % height_qc = surface_correction

      ENDIF

END  SUBROUTINE DA_Obs_Sfc_correction

SUBROUTINE INTPSFC_PRS (val, ho, po, hm, pm, tm, qm, to, qo)
!------------------------------------------------------------------------------!
!
! Correct pressure between two levels. 
!
! Reference: make use of the hydrosatic equation:
!
!  P2 = P1 * exp [-G/R * (z2-z1) / (tv1 + tv2)/2)
!
! Where:
!  z1  = height at level 1
!  z1  = height at level 2
!  tv1 = temperature at level 1
!  tv2 = temperature at level 2
!  P1  = Pressure at level 1
!  P2  = Pressure at level 2
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      REAL, INTENT (out)   :: val
      REAL, INTENT (in)    :: ho, po
      REAL, INTENT (in), OPTIONAL :: to, qo
      REAL, INTENT (in)    :: hm, pm, tm, qm    
      REAL                 :: tvo, tvm, tv, dz, arg

!------------------------------------------------------------------------------!

! 1.  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
! ---------------------------------------------

      tvm = tm  * (1. + 0.608 * qm)
      if (present(to) .and. present(qo)) then
        tvo = to  * (1. + 0.608 * qo)
      else if (present(to) .and. .not.present(qo)) then
        tvo = to
      else
        tvo = tvm
      endif

      tv  = 0.5 * (tvm + tvo)

! 2. HEIGHT DIFFERENCE BEWTEEN MODEL SURFACE AND OBSERVATIONS
! ------------------------------------------------------------

      dz = hm - ho

! 3.  EXTRAPOLATE PRESSURE OBS TO MODEL SURFACE
! ---------------------------------------------

      arg = dz * gravity / gas_constant
      arg = arg    / tv 

      val = po * exp (-arg)

END SUBROUTINE INTPSFC_PRS

SUBROUTINE INTPSFC_TEM (val, ho, po, to, height, pressure, temp, kts, kte)
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
            WRITE (0,'(A)') ' ERROR CANNOT FIND SFC + 200hPA AND SFC + 100hPA' 
            WRITE (0,'(A,I2,A,F10.3)') ' P (',k_200mb,') = ',prs_mb (k_200mb) 
            WRITE (0,'(A,I2,A,F10.3)') ' P (',k_100mb,') = ',prs_mb (k_100mb) 
            WRITE (0,'(A,F10.3)')      ' P_SFC  = ',         prs_mb (kts) 
            STOP 'in intpsfc_tem.F90'
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
      dhe_sfc1 = height (k_100mb )- height (kts)
      dhe_obs1 = height (k_100mb )- ho

      dhe_12   = height (k_200mb )- height (k_100mb)
      dhe_sfc2 = height (k_200mb )- height (kts)
      dhe_obs2 = height (k_200mb )- ho

! 3.2 Extrapolated potential temperature at model surface and observation loc
!     -----------------------------------------------------------------------

      th_sfc_int = th_100mb + (dth_21/dhe_12) * dhe_sfc1 
      th_obs_int = th_100mb + (dth_21/dhe_12) * dhe_obs1 


! 4.  BRING TEMPERATURE ONTO MODEL SURFACE
! ========================================

! 4.1 Difference at observation locations
!     -----------------------------------

      dth_obs = th_obs_int - th_obs

! 4.2 Difference at model surface
!     ---------------------------

      dth_sfc = (dhe_sfc2/dhe_obs2) * dth_obs

! 4.3 Potentiel temperature brought to model surface
!     ----------------------------------------------

      th_sfc = th_sfc_int - dth_sfc

! 4.3 Corresponding Temperature
!     -------------------------

      val  = th_sfc * (prs_mb (kts) / 1000.)**rcp

END SUBROUTINE INTPSFC_TEM

FUNCTION MO_CORRECTION (ho, po, to, qo, uo, vo, &
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

END FUNCTION MO_CORRECTION

   SUBROUTINE sfcprs (kts, kte, p, t, q, height, psfc, pslv, h_obs, t_obs)
   
   ! Purpose: to compute the pressure at obs height (psfc) from the sea
   !          level pressure (pslv), obs height (h_obs), temperature
   !          (t_obs, optional), and the model profile (p,t,q).
   !
   ! This subroutine is originally from MM5/INTERPF: module_diags.F.
   !  
   !                               Modified by Yong-Run Guo 10/31/00
    
      IMPLICIT NONE

      INTEGER,            intent(in)  :: kts, kte
      REAL, DIMENSION(kts:kte), intent(in)  :: HEIGHT, P, T, Q
      REAL,               intent(in)  :: H_OBS, PSLV    
      REAL,               intent(out) :: PSFC    
      REAL, OPTIONAL     ,intent(in)  :: T_OBS    

      REAL,          PARAMETER :: G = gravity
      REAL,          PARAMETER :: R =  gas_constant
      REAL,          PARAMETER :: ROV2 = R / 2.
      REAL,          PARAMETER :: gamma  = 6.5E-3, &   ! temperature Lapse rate
                         gammarg= gamma*gas_constant/g

      REAL,       dimension(3)    :: PLMB                   

      INTEGER                     :: K
      INTEGER                     :: K500
      INTEGER                     :: K700
      INTEGER                     :: K850

      LOGICAL                     :: L1
      LOGICAL                     :: L2
      LOGICAL                     :: L3

      REAL                        :: GAMMA78, GAMMA57  
      REAL                        :: HT    
      REAL                        :: P1 
      REAL                        :: P57
      REAL                        :: P78
      REAL                        :: T1       
      REAL                        :: T500       
      REAL                        :: T700    
      REAL                        :: T850    
      REAL                        :: TC
      REAL                        :: TFIXED 
      REAL                        :: TSLV, TSFC  

      TC   = 273.15 + 17.5
      PLMB = (/85000., 70000., 50000./)

      !  Find the locations of the 850, 700 and 500 mb levels.

101   K850 = 0                              ! FIND K AT: P=850
      K700 = 0                              !            P=700
      K500 = 0                              !            P=500

      DO K = kte-1, kts, -1
         IF      (p(k) > PLMB(1) .and. K850==0) THEN
            K850 = K + 1
         ELSE IF (p(k) > PLMB(2) .and. K700==0) THEN
            K700 = K + 1
         ELSE IF (p(k) > PLMB(3) .and. K500==0) THEN
            K500 = K + 1
         END IF
  
      END DO

      IF ( ( K850 .EQ. 0 ) .OR. ( K700 .EQ. 0 ) .OR. ( K500 .EQ. 0 ) ) THEN
!         PRINT '(A,3f8.0,A)','Error in finding p level for ',PLMB(1), &
!                             PLMB(2), PLMB(3),' Pa.'
!         DO K = 1, KX
!            PRINT '(A,I3,A,F10.2)','K = ',K,'  PRESSURE = ',P(K)
!         END DO
!         PRINT '(A,2f8.0,A,f8.0,A)','Expected ',    &
!               PLMB(1), PLMB(2),' and ',PLMB(3),' Pa. values, at least.'
!         STOP 'not_enough_levels'
!         PRINT '(A)','Change the pressure levels by -25mb'
         PLMB(1) = PLMB(1) - 2500.
         PLMB(2) = PLMB(2) - 2500.
         PLMB(3) = PLMB(3) - 2500.
         GO TO 101
      END IF

      !  The 850 hPa level is called something special, and interpolated
      !  to cross points.  And then, we fill those last rows and columns.

      ht = height(k850)

      !  The variable ht is now -h_obs/ht(850 hPa).  The plot thickens.

      ht = -h_obs / ht

      !  Make an isothermal assumption to get a first guess at the surface
      !  pressure.  This is to tell us which levels to use for the lapse
      !  rates in a bit.

      psfc = pslv * (pslv / p(k850)) ** ht

      !  Get a pressure more than 100 hPa above the surface - P1.  The
      !  P1 is the top of the level that we will use for our lapse rate
      !  computations.

            IF      ( (PSFC - p(k850) - 10000.) >= 0. ) THEN
               P1 = p(k850)
            ELSE IF ( (PSFC - p(k700)) >= 0. ) THEN
               P1 = PSFC - 10000.
            ELSE
               P1 = p(k500)
            END IF

      !  Compute virtual temperatures for k850, k700, and k500 layers.  Now
      !  you see why we wanted Q on pressure levels, it all is beginning   
      !  to make sense.

      t850 = t(k850) * (1. + 0.608 * q(k850))
      t700 = t(k700) * (1. + 0.608 * q(k700))
      t500 = t(k500) * (1. + 0.608 * q(k500))

      !  Compute two lapse rates between these three levels.  These are
      !  environmental values for each (i,j).

      gamma78 = LOG(t850 / t700)  / LOG (p(k850) / p(k700))
      gamma57 = LOG(t700 / t500)  / LOG (p(k700) / p(k500))

            IF ( ( psfc - p(k850) - 10000. ) >= 0. ) THEN
               t1 = t850
            ELSE IF ( ( psfc - p(k850) ) >= 0. ) THEN
               t1 = t700 * (p1 / p(k700)) ** gamma78
            ELSE IF ( ( psfc - p(k700) ) >= 0.) THEN 
               t1 = t500 * (p1 / p(k500)) ** gamma57
            ELSE
               t1 = t500
            ENDIF

      !  From our temperature way up in the air, we extrapolate down to
      !  the sea level to get a guess at the sea level temperature.

      tslv = t1 * (pslv / p1) ** (gammarg)

      !  The new surface temperature is computed from the with new sea level 
      !  temperature, just using the elevation and a lapse rate.  This lapse 
      !  rate is -6.5 K/km.

      if (PRESENT (t_obs)) then
        tsfc = t_obs
      else
        tsfc = tslv - gamma * h_obs
      endif

      !  A correction to the sea-level temperature, in case it is too warm.

      TFIXED = TC - 0.005 * (TSFC - TC) ** 2

            L1 = tslv .LT. tc
            L2 = tsfc .LE. tc
            L3 = .NOT. L1
            IF      ( L2 .AND. L3 ) THEN
               tslv = tc
            ELSE IF ( ( .NOT. L2 ) .AND. L3 ) THEN
               tslv = tfixed
            END IF

      !  Finally, we can get to the surface pressure.

      p1 = -h_obs * g / ( rov2 * ( tsfc + tslv ) )
      psfc = pslv * EXP(p1)

      !  Surface pressure and sea-level pressure are the same at sea level.

      IF ( ABS ( h_obs ) < 0.1 ) psfc = pslv

   END SUBROUTINE sfcprs

