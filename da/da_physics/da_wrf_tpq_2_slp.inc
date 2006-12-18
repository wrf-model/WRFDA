subroutine da_wrf_tpq_2_slp ( xb )

  implicit none

   !     SECTION  DIAGNOSTIC                                               
   !     PURPOSE  COMPUTES SEA LEVEL PRESSURE FROM THE RULE                
   !              T1/T2=(P1/P2)**(GAMMA*R/G).                              
   !                                                                       
   !     inPUT       T        TEMPERATURE                CROSS    3D       
   !                 Q        MIXinG RATIO               CROSS    2D       
   !                 P        PRESSURE PERTURBATION      CROSS    2D       
   !                 TERR     TERRAin                    CROSS    2D       
   !                 PSFC     SURFACE PRESSURE           CROSS    2D       
   !                                                                       
   !     outPUT      SLP      SEA LEVEL PRESSURE         CROSS    2D       
              

   type (xb_type), intent(inout) :: xb    ! first guess (local).

   integer              :: I, J, K, KLO, KHI
   real                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL

   real, parameter      :: GAMMA = 6.5E-3,  &
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
         end if

         PL  = xb%psfc(i,j) - PCONST                                        
         klo = 0

         ! FinD 2 LEVELS ON SIGMA SURFACES SURROUNDinG PL AT EACH I,J    

         k_loop: do k=xb%kts, xb%kte-1
            if ((xb%p(i,j,k) >= pl) .and. (xb%p(i,j,k+1) < pl)) then
               khi = k+1
               klo = k
               exit k_loop
            end if
         end do k_loop

         if (klo < 1) then                                      
            write(unit=message(1),fmt='(A,F11.3,A)') &
               'ERROR FinDinG PRESSURE LEVEL ',PCONST,' MB ABOVE THE SURFACE'
            write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') &
               'PL=',PL,'  PSFC=',xb%psfc(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))                                               
         end if                                                         

         ! GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
         ! AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

         TLO=xb%t(i,j,KLO) * (EPS+xb%q(i,j,KLO))/(EPS*(1.+xb%q(i,j,KLO)))
         THI=xb%t(i,j,KHI) * (EPS+xb%q(i,j,KHI))/(EPS*(1.+xb%q(i,j,KHI)))
         TL=THI-(THI-TLO)*LOG(PL/xb%p(i,j,KHI)) &
                      /LOG(xb%p(i,j,KLO)/xb%p(i,j,KHI))               
         TS=TL*(xb%psfc(i,j)/PL)**XTERM                           
         T0=TS +GAMMA*xb%terr(i,j)

         ! CORRECT SEA LEVEL TEMPERATURE if TOO HOT                      

         if ( t0 >= tc ) then
            if ( ts <= tc ) then
               t0 = tc
            else
               t0 = tc-0.005*(ts-tc)**2
            end if
         end if

         ! COMPUTE SEA LEVEL PRESSURE                                    

         xb%slp(i,j)=xb%psfc(i,j)*EXP(2.*gravity*xb%terr(i,j)/ &
            (gas_constant*(TS+T0)))
      end do i_loop
   end do j_loop

   if (trace_use) call da_trace_exit("da_wrf_tpq_2_slp")

end subroutine da_wrf_tpq_2_slp


