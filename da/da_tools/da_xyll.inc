subroutine da_xyll(XX,YY,XLAT,XLON)

   !-----------------------------------------------------------------------
   ! Purpose: calculates the latitudes and longitudes from the
   !                  (x,y) location (dot) in the mesoscale grids.
   ! on entry     :   
   ! x            : the coordinate in x (j)-direction.
   ! y            : the coordinate in y (i)-direction.
   !
   ! on exit      :                      
   ! xlat         : latitudes 
   ! xlon         : longitudes 
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: XX, YY
   real, intent(out) :: XLAT,XLON
        
   real              :: flp, flpp, r, cell, cel1, cel2
   real              :: rcone_factor, psx, conv
   real              :: cntri, cntrj, x, y, xcntr
   
   conv = 180.0 / pi

   ! seperate treatment for global fields
   if (fg_format == fg_format_kma_global) then
      xlat = yy * 180.0 /(coarse_jy-1)  -  90.0    
      xlon = xx * 360.0 /(coarse_ix-1)  - 180.0    
      return
   end if

   CNTRI = real(coarse_ix+1)/2.0
   CNTRJ = real(coarse_jy+1)/2.0

   xcntr = 0.0

   ! CALCULATE X AND Y POSITIONS OF GRID

   x = (xx - 0.5)*dsm/coarse_ds + start_x
   y = (yy - 0.5)*dsm/coarse_ds + start_y
   x = xcntr + (x-cntri)*coarse_ds
   y = ycntr + (y-cntrj)*coarse_ds

   ! NOW CALCULATE LAT AND LON OF THIS POinT

   if (map_projection.NE.3) then
      if(Y.EQ.0.0) then      
         if (X.GE.0.0) FLP =  90.0/CONV 
         if (X.LT.0.0) FLP = -90.0/CONV
      else
         if (PHIC.LT.0.0)then
            FLP = ATAN2(X,Y)   
         else
            FLP = ATAN2(X,-Y) 
         end if
      end if 
      FLPP = (FLP/cone_factor)*CONV+XLONC  
      if (FLPP.LT.-180.0) FLPP = FLPP + 360.0    
      if (FLPP.GT.180.0)  FLPP = FLPP - 360.0  
      XLON = FLPP 
      ! NOW SOLVE FOR LATITUDE
      R = sqrt(X*X+Y*Y)  
      if (PHIC.LT.0.0) R = -R  
      if (map_projection.EQ.1) then   
         CELL = (R*cone_factor)/(earth_radius*Sin(PSI1))    
         Rcone_factor  = 1.0/cone_factor   
         CEL1 = TAN(PSI1/2.0)*(CELL)**Rcone_factor    
      end if 
      if (map_projection.EQ.2) then
         CELL = R/earth_radius        
         CEL1 = CELL/(1.0+COS(PSI1))  
      end if 
      CEL2 = ATAN(CEL1)    
      PSX  = 2.0*CEL2*CONV
      XLAT = POLE-PSX 
   end if   
   ! CALCULATIONS FOR MERCATOR LAT,LON    
   if (map_projection.EQ.3) then   
      XLON = XLONC + ((X-XCNTR)/C2)*CONV 
      if (XLON.LT.-180.0) XLON = XLON + 360.0
      if (XLON.GT.180.0)  XLON = XLON - 360.0
      CELL = EXP(Y/C2)  
      XLAT = 2.0*(CONV*ATAN(CELL))-90.0 
   end if  

end subroutine da_xyll


