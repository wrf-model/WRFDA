SUBROUTINE da_set_map_para

   IMPLICIT NONE

   REAL            :: phictr, r, conv
   REAL            :: cell, cell2, psx

   conv = 180.0 / pi
                                                                  
   ! DEFINE PSI1:
   IF (map_projection.EQ.1 .OR. map_projection.EQ.2) THEN
      IF(PHIC.LT.0) THEN 
         PSI1 = 90.+TRUELAT1_3DV
         PSI1 = -PSI1
      ELSE
        PSI1 = 90.-TRUELAT1_3DV
      END IF          
   ELSE
      PSI1 = 0.
   END IF 

   PSI1 = PSI1/CONV
         
   ! CALCULATE R
   IF (map_projection.NE.3) THEN
      PSX = (POLE-PHIC)/CONV
      IF (map_projection.EQ.1) THEN
         CELL  = earth_radius*SIN(PSI1)/cone_factor  
         CELL2 = (TAN(PSX/2.))/(TAN(PSI1/2.))
      END IF 
      IF (map_projection.EQ.2) THEN
         CELL  = earth_radius*SIN(PSX)/cone_factor   
         CELL2 = (1. + COS(PSI1))/(1. + COS(PSX)) 
      END IF  
      R = CELL*(CELL2)**cone_factor 
      YCNTR = -R
   END IF  

   ! FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1
   IF (map_projection.EQ.3) THEN
      C2     = earth_radius*COS(PSI1) 
      PHICTR = PHIC/CONV 
      CELL   = COS(PHICTR)/(1.0+SIN(PHICTR)) 
      YCNTR  = - C2*LOG(CELL)  
   END IF 

END SUBROUTINE da_set_map_para
