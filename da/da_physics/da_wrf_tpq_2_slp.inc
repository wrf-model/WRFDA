subroutine da_wrf_tpq_2_slp (xb)

   !---------------------------------------------------------------------
   ! Purpose: computes sea level pressure from the rule                
   !          t1/t2=(p1/p2)**(gamma*r/g).                              
   !                                                                     
   ! input       t        temperature                cross    3d       
   !             q        mixing ratio               cross    2d       
   !             p        pressure perturbation      cross    2d       
   !             terr     terrain                    cross    2d       
   !             psfc     surface pressure           cross    2d       
   !                                                                       
   ! output      slp      sea level pressure         cross    2d    
   !---------------------------------------------------------------------

   implicit none   
              

   type (xb_type), intent(inout) :: xb    ! first guess (local).

   integer              :: I, J, K, KLO, KHI
   real                 :: PL, T0, TS, XTERM,    &
                          TLO, THI, TL

   real, parameter      :: GAMMA = 6.5E-3,  &
                          TC=273.15+17.5,  &
                          PCONST=10000. ,  &
                          EPS   = 0.622

   if (trace_use) call da_trace_entry("da_wrf_tpq_2_slp")

                                                                     
   ! sea level pressure                                            
                                                                         
   xterm=gamma* gas_constant / gravity                                                   
                                                                       
   ! compute pressure at pconst mb above surface (pl)              
                                                                          

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
               'Cannot find pressure levelL ',PCONST,' mb above the surface'
            write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') &
               'PL=',PL,'  PSFC=',xb%psfc(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))                                               
         end if                                                         

         ! get temperature at pl (tl), extrapolate t at surface (ts)     
         ! and t at sea level (t0) with 6.5 k/km lapse rate              

         tlo=xb%t(i,j,klo) * (eps+xb%q(i,j,klo))/(eps*(1.+xb%q(i,j,klo)))
         thi=xb%t(i,j,khi) * (eps+xb%q(i,j,khi))/(eps*(1.+xb%q(i,j,khi)))
         tl=thi-(thi-tlo)*log(pl/xb%p(i,j,khi)) &
                      /log(xb%p(i,j,klo)/xb%p(i,j,khi))               
         ts=tl*(xb%psfc(i,j)/pl)**xterm                           
         t0=ts +gamma*xb%terr(i,j)

         ! correct sea level temperature if too hot                      

         if (t0 >= tc) then
            if (ts <= tc) then
               t0 = tc
            else
               t0 = tc-0.005*(ts-tc)**2
            end if
         end if

         ! compute sea level pressure                                    

         xb%slp(i,j)=xb%psfc(i,j)*EXP(2.*gravity*xb%terr(i,j)/ &
            (gas_constant*(TS+T0)))
      end do i_loop
   end do j_loop

   if (trace_use) call da_trace_exit("da_wrf_tpq_2_slp")

end subroutine da_wrf_tpq_2_slp


