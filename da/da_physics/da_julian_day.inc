subroutine da_julian_day(NY,NM,ND,JD,METHOD) 
  
   !-----------------------------------------------------------------------
   ! METHOD = 1: inPUT ---- NY, NM, ND.  outPUT ---- JD                 C        
   ! METHOD = 2: inPUT ---- NY, JD.      outPUT ---- NM, ND             C 
   !----------------------------------------------------------------------- 
    
   implicit none

   integer,  intent(in)    :: METHOD, NY
   integer,  intent(inout) :: NM, ND, JD

   integer                 :: LOOP
   integer, dimension(12)  :: MDAY = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   if (MOD(NY,4) == 0) then
      MDAY(2)=29      
      if (MOD(NY,100) == 0) then
         MDAY(2)=28
         if (MOD(NY,400) == 0) then
            MDAY(2)=29
         end if
      end if
   end if

   if (METHOD == 1) then                  
      JD=ND
      JuDAY: do LOOP=1,NM-1                  
         JD=JD+MDAY(LOOP)                
      end do JuDAY                           
   else if (METHOD == 2) then             
      NM=1                               
      ND=JD
      do LOOP=1,11                    
         if (ND <= MDAY(LOOP)) exit

         ND=ND-MDAY(LOOP)     
         NM=NM+1                      
      end do
   end if                                

end subroutine da_julian_day


