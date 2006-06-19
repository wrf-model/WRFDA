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
! JRB
!      REAL    :: dth_12, dth_21, dth_sfc, dth_obs
!     REAL    :: dhe_12, dhe_21, dhe_sfc1, dhe_obs1, dhe_sfc2, dhe_obs2
      REAL    :: dth_21, dth_sfc, dth_obs
      REAL    :: dhe_12, dhe_sfc1, dhe_obs1, dhe_sfc2, dhe_obs2
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
            CALL wrf_errors(__FILE__,__LINE__,errmsg(1:4))
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
! JRB never used
!      dth_12 = th_200mb - th_100mb

! 3.1 Height increments
!     ----------------
    
! JRB never used
!      dhe_21   = height (k_100mb )- height (k_200mb)
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

END SUBROUTINE da_intpsfc_tem
