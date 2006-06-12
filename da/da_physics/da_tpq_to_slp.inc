SUBROUTINE da_tpq_to_slp ( T, Q, P, TERR, PSFC, SLP, xp )

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
  REAL,                intent(inout)            :: SLP

  INTEGER              :: I, J, K, KLO, KHI
  REAL                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL
                                               
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
        slp = psfc
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
         WRITE(UNIT=errmsg(1),FMT='(A,F11.3,A)') &
              'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
         WRITE(UNIT=errmsg(2),FMT='(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',psfc
         CALL wrf_errors(__FILE__,__LINE__,errmsg(1:2))                                
      END IF                                                         

! ... GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
!     AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

      TLO=t(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO)))
      THI=t(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))
      TL=THI-(THI-TLO)*LOG(PL/p(KHI)) &
                      /LOG(p(KLO)/p(KHI))               
      TS=TL*(psfc/PL)**XTERM                           
      T0=TS +GAMMA*terr

! ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      

      if ( t0 >= tc ) then
        if ( ts <= tc ) then
          t0 = tc
        else
          t0 = tc-0.005*(ts-tc)**2
        endif
      endif

! ... COMPUTE SEA LEVEL PRESSURE                                    

      slp=psfc*EXP(2.*gravity*terr/(gas_constant*(TS+T0)))

END SUBROUTINE da_tpq_to_slp
