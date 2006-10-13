subroutine da_wrf_tpq_2_slp ( xb )

  IMPLICIT NONE

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
              

   type (xb_type), intent(inout) :: xb    ! first guess (local).

   INTEGER              :: I, J, K, KLO, KHI
   REAL                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL

   REAL, PARAMETER      :: GAMMA = 6.5E-3,  &
                          TC=273.15+17.5,  &
                          PCONST=10000. ,  &
                          EPS   = 0.622

   if (trace_use) call da_trace_entry("da_wrf_tpq_2_slp")

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

         ! FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J    

         k_loop: do k=xb%kts, xb%kte-1
            if ((xb%p(i,j,k) >= pl) .and. (xb%p(i,j,k+1) < pl)) then
               khi = k+1
               klo = k
               exit k_loop
            endif
         enddo k_loop

         IF (klo < 1) THEN                                      
            WRITE(UNIT=errmsg(1),FMT='(A,F11.3,A)') &
               'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
            WRITE(UNIT=errmsg(2),FMT='(A,F11.3,2X,A,F11.3)') &
               'PL=',PL,'  PSFC=',xb%psfc(i,j)
            call da_error(__FILE__,__LINE__,errmsg(1:2))                                               
         END IF                                                         

         ! GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
         ! AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

         TLO=xb%t(i,j,KLO) * (EPS+xb%q(i,j,KLO))/(EPS*(1.+xb%q(i,j,KLO)))
         THI=xb%t(i,j,KHI) * (EPS+xb%q(i,j,KHI))/(EPS*(1.+xb%q(i,j,KHI)))
         TL=THI-(THI-TLO)*LOG(PL/xb%p(i,j,KHI)) &
                      /LOG(xb%p(i,j,KLO)/xb%p(i,j,KHI))               
         TS=TL*(xb%psfc(i,j)/PL)**XTERM                           
         T0=TS +GAMMA*xb%terr(i,j)

         ! CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      

         if ( t0 >= tc ) then
            if ( ts <= tc ) then
               t0 = tc
            else
               t0 = tc-0.005*(ts-tc)**2
            endif
         endif

         ! COMPUTE SEA LEVEL PRESSURE                                    

         xb%slp(i,j)=xb%psfc(i,j)*EXP(2.*gravity*xb%terr(i,j)/ &
            (gas_constant*(TS+T0)))
      enddo i_loop
   enddo j_loop

   if (trace_use) call da_trace_exit("da_wrf_tpq_2_slp")

end subroutine da_wrf_tpq_2_slp

