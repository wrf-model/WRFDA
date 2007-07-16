subroutine da_tpq_to_slp_lin ( T, Q, P, TERR, PSFC, T9, Q9, P9, PSFC9, SLP9)

   !-----------------------------------------------------------------------
   ! Purpose: computes sea level pressure from the rule                
   !              t1/t2=(p1/p2)**(gamma*r/g).                              
   !                                                                       
   !     input       t        temperature
   !                 q        mixing ratio
   !                 p        pressure
   !                 terr     terrain
   !                 psfc     surface pressure
   !                                                                       
   !     output      slp      sea level pressure
   !-----------------------------------------------------------------------

   implicit none
              
   real,                intent(in)               :: TERR, PSFC, PSFC9
   real, dimension(kms:kme), intent(in)    :: T, Q, P
   real, dimension(kms:kme), intent(in)    :: T9, Q9, P9
   ! real                                          :: SLP
   real, intent(out)                             :: SLP9

   integer              :: K, KLO, KHI
   real                 :: PL, T0, TS, XTERM,    &
                       TLO, THI, TL
   real                 :: PL9,T09,TS9,TLO9,THI9,TL9,COEF1,COEF2

   real, parameter      :: GAMMA = 6.5E-3,  &
                       TC=t_kelvin+17.5,  &
                       PCONST=10000. ,  &
                       EPS   = 0.622
                                                                          
   !     ... SEA LEVEL PRESSURE                                            
                                                                          
   XTERM=GAMMA* gas_constant / gravity                                                   
                                                                          
   ! COMPUTE PRESSURE AT PCONST MB ABOVE SURFACE (PL)                                                                                     

   if (terr <= 0.) then
      slp9 = psfc9
      ! slp = psfc
      return
   end if

   PL9  = psfc9 
   PL  = psfc - PCONST                                        
   klo = 0

   ! FinD 2 LEVELS ON SIGMA SURFACES SURROUNDinG PL AT EACH I,J    

   k_loop: do k=kts, kte-1
      if ((p(k) >= pl) .and. (p(k+1) < pl)) then
         khi = k+1
         klo = k
         exit k_loop
      end if
   end do k_loop

   if(klo < 1) then                                      
      write(unit=message(1),fmt='(A,F11.3,A)') &
           'ERROR FinDinG PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
      write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',psfc
      call da_error(__FILE__,__LINE__,message(1:2))                                               
   end if                                                         

   ! GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
   ! AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

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

   ! CORRECT SEA LEVEL TEMPERATURE if TOO HOT                      

   if ( t0 >= tc ) then
      if ( ts <= tc ) then
         t09 = 0.0
         t0 = tc
      else
         t09 = -0.01*(ts-tc)*ts9
         t0 = tc-0.005*(ts-tc)**2
      end if
   end if

   ! COMPUTE SEA LEVEL PRESSURE                                    

   slp9=psfc9*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))  &
          -psfc*EXP(2.*gravity*TERR/(gas_constant*(TS+T0)))*  &
          2.*gravity*TERR/(gas_constant*(TS+T0)**2)*(TS9+T09)
   ! slp=psfc*EXP(2.*gravity*terr/(gas_constant*(TS+T0)))

end subroutine da_tpq_to_slp_lin


