SUBROUTINE DA_TPQ_To_SLP_Lin ( T, Q, P, TERR, PSFC, &
                              T9, Q9, P9, PSFC9, SLP9, xp )

  IMPLICIT NONE
!                                                                       
!     SECTION  DIAGNOSTIC                                               
!     PURPOSE  COMPUTES SEA LEVEL PRESSURE FROM THE RULE                
!              T1/T2=(P1/P2)**(GAMMA*R/G).                              
!                                                                       
!     INPUT       T        TEMPERATURE
!                 Q        MIXING RATIO
!                 P        PRESSURE
!                 TERR     TERRAIN
!                 PSFC     SURFACE PRESSURE
!                                                                       
!     OUTPUT      SLP      SEA LEVEL PRESSURE
!              

  TYPE (xpose_type), INTENT(IN)                 :: xp
  REAL,                intent(in)               :: TERR, PSFC, PSFC9
  REAL, dimension(xp%kms:xp%kme), intent(in)    :: T, Q, P
  REAL, dimension(xp%kms:xp%kme), intent(in)    :: T9, Q9, P9
  REAL                                          :: SLP, SLP9

  INTEGER              :: I, J, K, KLO, KHI
  REAL                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL
  REAL                 :: PL9,T09,TS9,TLO9,THI9,TL9,COEF1,COEF2
                                               
  REAL, PARAMETER      :: GAMMA = 6.5E-3,  &
                          TC=273.15+17.5,  &
                          PCONST=10000. ,  &
                          EPS   = 0.622
!                                                                       
!     ... SEA LEVEL PRESSURE                                            
!                                                                       
      XTERM=GAMMA* gas_constant / gravity                                                   
!                                                                       
!     ... COMPUTE PRESSURE AT PCONST MB ABOVE SURFACE (PL)              
!                                                                       
      
      if (terr <= 0.) then
        slp9 = psfc9
        slp = psfc
        return
      endif

      PL9  = psfc9 
      PL  = psfc - PCONST                                        
      klo = 0

! ... FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J    

      k_loop: do k=xp%kts, xp%kte-1
         if ((p(k) >= pl) .and. (p(k+1) < pl)) then
            khi = k+1
            klo = k
            exit k_loop
         endif
      enddo k_loop

      IF(klo < 1) THEN                                      
         WRITE(0,'(A,F11.3,A)') &
              'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
         WRITE(0,'(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',psfc
         CALL ABORT                                               
      END IF                                                         

! ... GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
!     AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

      TLO9=t9(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO))) + &
           q9(KLO)*t(KLO)*(1.0-EPS)/(EPS*(1.+q(KLO))**2)
      TLO=t(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO)))
      THI9=t9(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))+   &
           q9(KHI)*t(KHI)*(1.0-EPS)/(EPS*(1.+q(KHI))**2)
      THI=t(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))
      COEF1=ALOG(PL/p(KHI))
      COEF2=ALOG(p(KLO)/p(KHI))
      TL9=(1.0-COEF1/COEF2)*THI9+COEF1/COEF2*TLO9       &
          -(THI-TLO)/(COEF2*PL)*PL9                 &
          +((THI-TLO)/(COEF2*p(KHI))*(1.-COEF1/COEF2))*p9(KHI)   &
          +(THI-TLO)*COEF1/(COEF2*COEF2*p(KLO))*p9(KLO)
      TL=THI-(THI-TLO)*COEF1/COEF2
      TS9=TL9*(psfc/PL)**XTERM+psfc9*XTERM*(TL/PL)*(psfc/PL)**  &
          (XTERM-1)-PL9*XTERM*(TL*psfc/(PL*PL))*(psfc/PL)**(XTERM-1)
      TS=TL*(psfc/PL)**XTERM                           
      T09=TS9
      T0=TS +GAMMA*terr

! ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      

      if ( t0 >= tc ) then
        if ( ts <= tc ) then
          t09 = 0.0
          t0 = tc
        else
          t09 = -0.01*(ts-tc)*ts9
          t0 = tc-0.005*(ts-tc)**2
        endif
      endif

! ... COMPUTE SEA LEVEL PRESSURE                                    

      slp9=psfc9*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))  &
             -psfc*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))*  &
             2.*gravity*TERR/(gas_constant*(TS+T0)**2)*(TS9+T09)
!     slp=psfc*EXP(2.*gravity*terr/(gas_constant*(TS+T0)))

END SUBROUTINE DA_TPQ_To_SLP_Lin
