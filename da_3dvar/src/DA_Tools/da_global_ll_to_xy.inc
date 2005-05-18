subroutine da_global_ll_to_xy(lat,lon,x,y)
!------------------------------------------------------------------------------!
!
! PURPOSE:  CALCULATES THE (X,Y) LOCATION (DOT) IN THE GLOBAL GRIDS
! -------   FROM LATITUDES AND LONGITUDES
!
! Wei Huang
! March 31, 2005
!
! INPUT:
! -----
! LAT:    LATITUDES
! LON:    LONGITUDES
!
! OUTPUT:
! -----
! X:      THE COORDINATE IN X (I)-DIRECTION.
! Y:      THE COORDINATE IN Y (J)-DIRECTION.
!------------------------------------------------------------------------------!
   
   implicit none
   
   real, intent(in)  :: lat, lon
   real, intent(out) :: x, y

   real              :: xlat, xlon
   
   xlat = lat - start_lat
   xlon = lon - start_lon

   if (xlat < 0.0) xlat = xlat + 180.0
   if (xlon < 0.0) xlon = xlon + 360.0

   x = start_x + xlon/delt_lon
   y = start_y + xlat/delt_lat
   
end subroutine da_global_ll_to_xy

