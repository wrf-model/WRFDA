subroutine llxy (xlati,xloni,x,y)
!------------------------------------------------------------------------------!
!
!                 ROUTINE LLXY
!                **************
!
!
! PURPOSE:  CALCULATES THE (X,Y) LOCATION (DOT) IN THE MESOSCALE GRIDS
! -------   FROM LATITUDES AND LONGITUDES
!
!  Additions:      10/26/2004    Syed RH Rizvi
!                  Global domain co-ordinates
!
!           For global domain co-ordinates
!
!  INPUT:
!  -----
!   XLAT:    LATITUDES
!   XLON:    LONGITUDES
!
! OUTPUT:
! -----
!   X:        THE COORDINATE IN X (I)-DIRECTION.
!   Y:        THE COORDINATE IN Y (J)-DIRECTION.
!
!------------------------------------------------------------------------------!
   
   implicit none
   
   real, intent(in)  :: xlati, xloni
   real, intent(out) :: x, y

   real              :: dxlon
   real              :: xlat, xlon
   real              :: xx, yy, xc, yc
   real              :: cell, psi0, psx, r, flp
   real              :: centri, centrj
   real              :: ratio
   real              :: bb
   real, parameter   :: conv = 180.0 / pi
   
!------------------------------------------------------------------------------!

   xlon = xloni
   xlat = xlati

   xlat = max (xlat, -89.95)
   xlat = min (xlat, +89.95)
   
   dxlon = xlon - xlonc
   if (dxlon >  180) dxlon = dxlon - 360.
   if (dxlon < -180) dxlon = dxlon + 360.
   
!------------------------------------------------------------------------------!

   if (map_projection == 3) then
      xc = 0.0
      yc = YCNTR

      cell = cos(xlat/conv)/(1.0+sin(xlat/conv))
      yy = -c2*alog(cell)
      xx = c2*dxlon/conv

   else

      psi0 = ( pole - phic )/conv
      xc = 0.0

!-----CALCULATE X,Y COORDS. RELATIVE TO POLE

      flp = cone_factor*dxlon/conv
   
      psx = ( pole - xlat )/conv
   
      if (map_projection == 2) then
! ...... Polar stereographics:
         bb = 2.0*(cos(psi1/2.0)**2)
         yc = -earth_radius*bb*tan(psi0/2.0)
          r = -earth_radius*bb*tan(psx/2.0)
      else
! ...... Lambert conformal:
         bb = -earth_radius/cone_factor*sin(psi1)
         yc = bb*(tan(psi0/2.0)/tan(psi1/2.0))**cone_factor
          r = bb*(tan(psx /2.0)/tan(psi1/2.0))**cone_factor
      endif

      if (phic < 0.0) then
         xx = r*sin(flp)
         yy = r*cos(flp)
      else
         xx = -r*sin(flp)
         yy =  r*cos(flp)
      endif

   endif

! TRANSFORM (1,1) TO THE ORIGIN
! the location of the center in the coarse domain

   centri = real (coarse_ix + 1)/2.0  
   centrj = real (coarse_jy + 1)/2.0  

! the (X,Y) coordinates in the coarse domain

   x = ( xx - xc )/coarse_ds + centri 
   y = ( yy - yc )/coarse_ds + centrj  

   ratio = coarse_ds / dsm

!--only add 0.5 so that x/y is relative to first cross points (MM5 input):

   x = (x - start_x)*ratio + 0.5
   y = (y - start_y)*ratio + 0.5
end subroutine llxy

