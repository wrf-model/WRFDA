subroutine da_set_map_para

   implicit none

   real            :: phictr, r, conv
   real            :: cell, cell2, psx

   conv = 180.0 / pi
                                                                  
   ! DEFinE PSI1:
   if (map_projection.EQ.1 .OR. map_projection.EQ.2) then
      if(PHIC.LT.0) then 
         PSI1 = 90.+trueLAT1_3DV
         PSI1 = -PSI1
      else
        PSI1 = 90.-trueLAT1_3DV
      end if          
   else
      PSI1 = 0.
   end if 

   PSI1 = PSI1/CONV
         
   ! CALCULATE R
   if (map_projection.NE.3) then
      PSX = (POLE-PHIC)/CONV
      if (map_projection.EQ.1) then
         CELL  = earth_radius*Sin(PSI1)/cone_factor  
         CELL2 = (TAN(PSX/2.))/(TAN(PSI1/2.))
      end if 
      if (map_projection.EQ.2) then
         CELL  = earth_radius*Sin(PSX)/cone_factor   
         CELL2 = (1. + COS(PSI1))/(1. + COS(PSX)) 
      end if  
      R = CELL*(CELL2)**cone_factor 
      YCNTR = -R
   end if  

   ! FOR MERCATOR PROJECTION, THE PROJECTION IS true AT LAT AT PHI1
   if (map_projection.EQ.3) then
      C2     = earth_radius*COS(PSI1) 
      PHICTR = PHIC/CONV 
      CELL   = COS(PHICTR)/(1.0+Sin(PHICTR)) 
      YCNTR  = - C2*LOG(CELL)  
   end if 

end subroutine da_set_map_para


