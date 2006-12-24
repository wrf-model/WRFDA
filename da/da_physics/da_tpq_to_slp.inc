subroutine da_tpq_to_slp ( T, Q, P, TERR, PSFC, SLP, xp )

   !-----------------------------------------------------------------------
   ! Purpose:  computes sea level pressure from the rule                
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

   type (xpose_type), intent(in)                 :: xp
   real,                intent(in)               :: TERR, PSFC
   real, dimension(xp%kms:xp%kme), intent(in)    :: T, Q, P
   real,                intent(inout)            :: SLP

   integer              :: K, KLO, KHI
   real                 :: PL, T0, TS, XTERM,    &
                           TLO, THI, TL
                                               
   real, parameter      :: GAMMA = 6.5E-3,  &
                           TC=273.15+17.5,  &
                           PCONST=10000. ,  &
                           EPS   = 0.622
                                                                       
   ! SEA LEVEL PRESSURE                                            
                                                                         
   XTERM=GAMMA* gas_constant / gravity                                                   
                                                                       
   ! COMPUTE PRESSURE AT PCONST MB ABOVE SURFACE (PL)              
                                                                        
   if (terr <= 0.) then
      slp = psfc
      return
   end if

   PL  = psfc - PCONST                                        
   klo = 0

   ! FinD 2 LEVELS ON SIGMA SURFACES SURROUNDinG PL AT EACH I,J    

   do k=xp%kts, xp%kte-1
      if ((p(k) >= pl) .and. (p(k+1) < pl)) then
         khi = k+1
         klo = k
         exit
      end if
   end do

   if (klo < 1) then                                      
      write(unit=message(1),fmt='(A,F11.3,A)') &
         'ERROR FinDinG PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
      write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',psfc
      call da_error(__FILE__,__LINE__,message(1:2))                                
   end if                                                         

   ! GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
   ! AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

   TLO=t(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO)))
   THI=t(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))
   TL=THI-(THI-TLO)*LOG(PL/p(KHI)) &
                      /LOG(p(KLO)/p(KHI))               
   TS=TL*(psfc/PL)**XTERM                           
   T0=TS +GAMMA*terr

   ! CORRECT SEA LEVEL TEMPERATURE if TOO HOT                      

   if ( t0 >= tc ) then
      if ( ts <= tc ) then
        t0 = tc
      else
        t0 = tc-0.005*(ts-tc)**2
      end if
   end if

   ! COMPUTE SEA LEVEL PRESSURE                                    

   slp=psfc*EXP(2.*gravity*terr/(gas_constant*(TS+T0)))

end subroutine da_tpq_to_slp


