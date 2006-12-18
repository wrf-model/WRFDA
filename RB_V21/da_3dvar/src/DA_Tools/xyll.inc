SUBROUTINE XYLL(XX,YY,XLAT,XLON)
!               
!   PURPOSE      : CALCULATES THE LATITUDES AND LONGITUDES FROM THE
!                  (X,Y) LOCATION (DOT) IN THE MESOSCALE GRIDS.
!   ON ENTRY     :   
!   X            : THE COORDINATE IN X (J)-DIRECTION.
!   Y            : THE COORDINATE IN Y (I)-DIRECTION.
!
!   ON EXIT      :                      
!   XLAT         : LATITUDES 
!   XLON         : LONGITUDES 
!

   IMPLICIT NONE

   REAL, INTENT(IN)  :: XX, YY
   REAL, INTENT(OUT) :: XLAT,XLON
        
   REAL              :: flp, flpp, r, cell, cel1, cel2
   REAL              :: rcone_factor, psx, conv
   REAL              :: cntri, cntrj, x, y, xcntr
   
   conv = 180.0 / pi

! seperate treatment for global fields
   if(fg_format == 3) then
      xlat = yy * 180.0 /(coarse_jy-1)  -  90.0    
      xlon = xx * 360.0 /(coarse_ix-1)  - 180.0    
      return
   endif

   CNTRI = real(coarse_ix+1)/2.
   CNTRJ = real(coarse_jy+1)/2. 

   xcntr = 0.0

!-----CALCULATE X AND Y POSITIONS OF GRID

   x = (xx - 0.5)*dsm/coarse_ds + start_x
   y = (yy - 0.5)*dsm/coarse_ds + start_y
   x = xcntr + (x-cntri)*coarse_ds
   y = ycntr + (y-cntrj)*coarse_ds

!-----NOW CALCULATE LAT AND LON OF THIS POINT

   IF (map_projection.NE.3) THEN
      IF(Y.EQ.0.) THEN      
        IF(X.GE.0.0) FLP =  90.0/CONV 
        IF(X.LT.0.0) FLP = -90.0/CONV
      ELSE
        IF (PHIC.LT.0.0)THEN
            FLP = ATAN2(X,Y)   
        ELSE
            FLP = ATAN2(X,-Y) 
        ENDIF
      ENDIF 
      FLPP = (FLP/cone_factor)*CONV+XLONC  
      IF (FLPP.LT.-180.) FLPP = FLPP + 360    
      IF (FLPP.GT.180.)  FLPP = FLPP - 360.  
      XLON = FLPP 
!--------NOW SOLVE FOR LATITUDE
      R = SQRT(X*X+Y*Y)  
      IF (PHIC.LT.0.0) R = -R  
      IF (map_projection.EQ.1) THEN   
         CELL = (R*cone_factor)/(earth_radius*SIN(PSI1))    
         Rcone_factor  = 1.0/cone_factor   
         CEL1 = TAN(PSI1/2.)*(CELL)**Rcone_factor    
      ENDIF 
      IF (map_projection.EQ.2) THEN
         CELL = R/earth_radius        
         CEL1 = CELL/(1.0+COS(PSI1))  
      ENDIF 
      CEL2 = ATAN(CEL1)    
      PSX  = 2.*CEL2*CONV
      XLAT = POLE-PSX 
   ENDIF   
!-----CALCULATIONS FOR MERCATOR LAT,LON    
   IF (map_projection.EQ.3) THEN   
      XLON = XLONC + ((X-XCNTR)/C2)*CONV 
      IF (XLON.LT.-180.) XLON = XLON + 360
      IF (XLON.GT.180.)  XLON = XLON - 360.
      CELL = EXP(Y/C2)  
      XLAT = 2.*(CONV*ATAN(CELL))-90.0 
   ENDIF  

END SUBROUTINE XYLL

