subroutine da_sfcprs (kts, kte, p, t, q, height, psfc, pslv, h_obs, t_obs)
   
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
!       WRITE(UNIT=errmsg(1),FMT='(A,3f8.0,A)') &
!         'Error in finding p level for ',PLMB(1), PLMB(2), PLMB(3),' Pa.'
!         DO K = 1, KX
!           WRITE(UNIT=errmsg(k+1),FMT='(A,I3,A,F10.2)') 'K = ',K,'  PRESSURE = ',P(K)
!         END DO
!         WRITE(UNIT=errmsg(kx+2),FMT='(A,2f8.0,A,f8.0,A)) ','Expected ',    &
!           PLMB(1), PLMB(2),' and ',PLMB(3),' Pa. values, at least.'
!         errmsg(kx+3)="not enough levels"
!         errmsg(kx+4)="Change the pressure levels by -25mb"
!         call da_error(__FILE__,__LINE__,errmsg(1:kx+4))
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

   end subroutine da_sfcprs

