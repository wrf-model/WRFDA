SUBROUTINE DA_WRF_TPQ_2_SLP ( xb )

  IMPLICIT NONE
!                                                                       
!     SECTION  DIAGNOSTIC                                               
!     PURPOSE  COMPUTES SEA LEVEL PRESSURE FROM THE RULE                
!              T1/T2=(P1/P2)**(GAMMA*R/G).                              
!                                                                       
!     INPUT       T        TEMPERATURE                CROSS    3D       
!                 Q        MIXING RATIO               CROSS    2D       
!                 P        PRESSURE PERTURBATION      CROSS    2D       
!                 TERR     TERRAIN                    CROSS    2D       
!                 PSFC     SURFACE PRESSURE           CROSS    2D       
!                                                                       
!     OUTPUT      SLP      SEA LEVEL PRESSURE         CROSS    2D       
!              

  type (xb_type), intent(inout) :: xb    ! first guess (local).

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
      
   j_loop: do j=xb%jts, xb%jte
   i_loop: do i=xb%its, xb%ite
      if (xb%terr(i,j) <= 0.) then
        xb%slp(i,j) = xb%psfc(i,j)
        cycle i_loop
      endif

      PL  = xb%psfc(i,j) - PCONST                                        
      klo = 0

! ... FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J    

      k_loop: do k=xb%kts, xb%kte-1
         if ((xb%p(i,j,k) >= pl) .and. (xb%p(i,j,k+1) < pl)) then
            khi = k+1
            klo = k
            exit k_loop
         endif
      enddo k_loop

      IF(klo < 1) THEN                                      
         WRITE(0,'(A,F11.3,A)') &
              'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
         WRITE(0,'(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',xb%psfc(i,j)
         CALL ABORT                                               
      END IF                                                         

! ... GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
!     AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

      TLO=xb%t(i,j,KLO) * (EPS+xb%q(i,j,KLO))/(EPS*(1.+xb%q(i,j,KLO)))
      THI=xb%t(i,j,KHI) * (EPS+xb%q(i,j,KHI))/(EPS*(1.+xb%q(i,j,KHI)))
      TL=THI-(THI-TLO)*LOG(PL/xb%p(i,j,KHI)) &
                      /LOG(xb%p(i,j,KLO)/xb%p(i,j,KHI))               
      TS=TL*(xb%psfc(i,j)/PL)**XTERM                           
      T0=TS +GAMMA*xb%terr(i,j)

! ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      

      if ( t0 >= tc ) then
        if ( ts <= tc ) then
          t0 = tc
        else
          t0 = tc-0.005*(ts-tc)**2
        endif
      endif

! ... COMPUTE SEA LEVEL PRESSURE                                    

      xb%slp(i,j)=xb%psfc(i,j)*EXP(2.*gravity*xb%terr(i,j)/(gas_constant*(TS+T0)))
   enddo i_loop
   enddo j_loop

END SUBROUTINE DA_WRF_TPQ_2_SLP

SUBROUTINE DA_TPQ_to_SLP (T, QV, PP, TER, PS, SLP, sigmah )

  IMPLICIT NONE
!                                                                       
!     SECTION  DIAGNOSTIC                                               
!     PURPOSE  COMPUTES SEA LEVEL PRESSURE FROM THE RULE                
!              T1/T2=(P1/P2)**(GAMMA*R/G).                              
!                                                                       
!     INPUT       T        TEMPERATURE                CROSS    3D       
!                 QV       MIXING RATIO               CROSS    2D       
!                 PP       PRESSURE PERTURBATION      CROSS    2D       
!                 TER      TERRAIN                    CROSS    2D       
!                 PS       P STAR = PSFC-PTOP         CROSS    2D       
!                 SFP      SURFACE PRESSURE           CROSS    2D       
!                 SIG      HALF SIGMA LEVELS                   1D       
!                 IMX      DOT POINT DIMENSION N-S                      
!                 JMX      DOT POINT DIMENSION E-W                      
!                 KX       NUMBER OF VERTICAL LEVELS                    
!                 PTOP     PRESSURE AT TOP OF MODEL                     
!                                                                       
!     OUTPUT      SLP      SEA LEVEL PRESSURE         CROSS    2D       
!              
  REAL,                intent(in)    :: TER, PS
  REAL, dimension(:),  intent(in)    :: T, QV, PP, sigmah
  REAL,                intent(inout) :: SLP

  INTEGER              :: K, KUPTO, KLO, KHI, KLEV
  REAL                 :: SFP, PL, T0, TS, XTERM,    &
                          PLO, PHI, TLO, THI, TL, TBAR, HL
  LOGICAL              :: L1,L2,L3,L4
                                               
  REAL, PARAMETER      ::  GAMMA = 6.5E-3,  &
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

      KUPTO=size(t)/2 - 1

99    CONTINUE                                                          
      SFP  = (1.0-sigmah(1))*ps + PP (1)
      PL   = SFP - PCONST                                        
      KLEV = 1                                                  
!                                                                       
!     ... FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J    
!                                                                       

      DO K=1,KUPTO
        IF(((SIGMAH(K  )*PS+PTOP) >= PL) .AND.     & 
           ((SIGMAH(K+1)*PS+PTOP) < PL)) THEN
            KLEV=K                                         
            exit
        ENDIF
      ENDDO 
                                                      
      IF(KLEV < 1) THEN                                      
        WRITE(0,'(A,F11.3,A)') &
        'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
        WRITE(0,'(A,I3)') 'LAST K LEVEL =',KUPTO                              
        IF(KUPTO < size(t)) THEN                                         
          KUPTO=2*KUPTO
          WRITE(0,'(A, I4)') 'TRYING AGAIN WITH KUPTO=', KUPTO
          GOTO 99                                                  
        ELSE                                                        
          WRITE(0,'(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',SFP
          CALL ABORT                                               
        END IF                                                      
      END IF                                                         
!                                                                       
!     ... GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
!         AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              
!                                                                       

      KLO=KLEV
      KHI=KLEV+1
      PLO=SIGMAH(KLO)*PS+PTOP                                      
      PHI=SIGMAH(KHI)*PS+PTOP                                      
      TLO=T(KLO) * (EPS+QV(KLO))/(EPS*(1.+QV(KLO)))
      THI=T(KHI) * (EPS+QV(KHI))/(EPS*(1.+QV(KHI)))
      TL=THI-(THI-TLO)*LOG(PL/PHI)/LOG(PLO/PHI)               
      TS=TL*(SFP/PL)**XTERM                           
      TBAR=(TS+TL)*0.5                                          
      HL=TER-gas_constant/gravity*LOG(PL/SFP)*TBAR                    
      T0=TS +GAMMA*TER
!                                                                       
!     ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      
!                                                                       
      L1=T0.LT.TC                                               
      L2=TS.LE.TC                                               
      L3=.NOT.L1                                                     
      IF(L2.AND.L3)THEN                                              
        T0=TC                                                  
      ELSE IF((.NOT. L2).AND. L3) THEN                               
        T0=TC-0.005*(TS-TC)**2                            
      ENDIF                                                          
!                                                                       
!     ... COMPUTE SEA LEVEL PRESSURE                                    
!
      if (ter <= 0.) then
        SLP = SFP
      else
        SLP=SFP*EXP(2.*gravity*TER/(gas_constant*(TS+T0)))
      endif

END SUBROUTINE DA_TPQ_to_SLP

