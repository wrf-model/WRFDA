SUBROUTINE DA_TPQ_To_SLP_Adj ( T, Q, P, TERR, PSFC, &
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
  REAL,                intent(in)               :: TERR, PSFC
  REAL, dimension(xp%kms:xp%kme), intent(in)    :: T, Q, P
  REAL, dimension(xp%kms:xp%kme)                :: T9, Q9, P9
  REAL                                          :: SLP, SLP9, PSFC9

  INTEGER              :: I, J, K, KLO, KHI
  REAL                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL
  REAL                 :: PL9,T09,TS9,TLO9,THI9,TL9,COEF1,COEF2
  REAL                 :: T08,TS8
                                               
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
      T9(:)=0.
      Q9(:)=0.
      P9(:)=0.
      
      if (terr <= 0.) then
        psfc9=slp9
        return
      endif

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

      TLO=t(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO)))
      THI=t(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))
      COEF1=ALOG(PL/p(KHI))
      COEF2=ALOG(p(KLO)/p(KHI))
      TL=THI-(THI-TLO)*COEF1/COEF2
      TS=TL*(psfc/PL)**XTERM                           
      TS8=TS
      T0=TS +GAMMA*terr
      T08=T0

! ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      

      if ( t0 >= tc ) then
        if ( ts <= tc ) then
          t0 = tc
        else
          t0 = tc-0.005*(ts-tc)**2
        endif
      endif
!
      psfc9=slp9*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))
      TS9=-slp9*psfc*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))*  &
          2.*gravity*TERR/(gas_constant*(TS+T0)**2)
      T09=-slp9*psfc*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))*  &
          2.*gravity*TERR/(gas_constant*(TS+T0)**2)

      if ( t08 >= tc ) then
        if ( ts8 > tc ) then
          ts9=ts9-0.01*(ts-tc)*t09
        endif
        t09=0.
      endif

      TS9=TS9+T09
      TL9=TS9*(psfc/PL)**XTERM
      psfc9=psfc9+TS9*XTERM*(TL/PL)*(psfc/PL)**(XTERM-1)      
      PL9=-TS9*XTERM*(TL*psfc/(PL*PL))*(psfc/PL)**(XTERM-1)
      THI9=(1.0-COEF1/COEF2)*TL9
      TLO9=COEF1/COEF2*TL9
      PL9=PL9-(THI-TLO)/(COEF2*PL)*TL9
      p9(KHI)=p9(KHI)+((THI-TLO)/(COEF2*p(KHI))*(1.-COEF1/COEF2))*TL9
      p9(KLO)=p9(KLO)+(THI-TLO)*COEF1/(COEF2*COEF2*p(KLO))*TL9
      t9(KHI)=t9(KHI)+THI9* (EPS+q(KHI))/(EPS*(1.+q(KHI)))
      q9(KHI)=q9(KHI)+THI9*t(KHI)*(1.0-EPS)/(EPS*(1.+q(KHI))**2)
      t9(KLO)=t9(KLO)+TLO9* (EPS+q(KLO))/(EPS*(1.+q(KLO)))
      q9(KLO)=q9(KLO)+TLO9*t(KLO)*(1.0-EPS)/(EPS*(1.+q(KLO))**2)

      psfc9=psfc9+PL9

END SUBROUTINE DA_TPQ_To_SLP_Adj
