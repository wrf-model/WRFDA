subroutine da_earth_2_model_wind(eu,ev,mu,mv,lon)

   !---------------------------------------------------------------------------
   ! Convert earth wind to model wind.
   !
   ! Need map projection parameters.
   !
   ! IPROJ: Projection type
   ! PHIC:  Central latitude 
   ! XLONC: Central longitude
   ! XN:    Cone projection
   ! CONV:  180/Pi
   !
   !---------------------------------------------------------------------------

   implicit none

   real,    intent (in)  :: eu, ev
   real,    intent (out) :: mu, mv
   real,    intent (in)  :: lon

   real :: XLONRT, ANG

   ! FOR MERCATOR PROJECTION, THE WinDS ARE AS in EARTH COORDinATES

   if(map_projection == 3) then
      mu = eu
      mv = ev
      return
   end if

   ! FOR CONVERSION TO GRID COORDinATES,
   ! SEE program DATAMAP, SUBR VECT, AND
   ! ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

   XLONRT = XLONC-LON

   if (XLONRT > 180.) XLONRT=XLONRT-360.
   if (XLONRT <-180.) XLONRT=XLONRT+360.

   ANG=XLONRT*CONE_FACTOR*pi/180.0

   if(PHIC < 0.0) ANG=-ANG

   mu = ev*Sin(ANG) + eu*COS(ANG)
   mv = ev*COS(ANG) - eu*Sin(ANG)

end subroutine da_earth_2_model_wind

