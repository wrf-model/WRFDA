SUBROUTINE earth_2_model_wind(eu,ev,mu,mv,lon)

!------------------------------------------------------------------------------!
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
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    REAL,    INTENT (in)  :: eu, ev
    REAL,    INTENT (out) :: mu, mv
    REAL,    INTENT (in)  :: lon

    REAL :: XLONRT, ANG

!------------------------------------------------------------------------------!

!   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES

    IF(map_projection == 3) then
       mu = eu
       mv = ev
       return
    endif

!     FOR CONVERSION TO GRID COORDINATES,
!     SEE PROGRAM DATAMAP, SUBR VECT, AND
!     ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

      XLONRT = XLONC-LON

      IF(XLONRT > 180.) XLONRT=XLONRT-360.
      IF(XLONRT <-180.) XLONRT=XLONRT+360.

      ANG=XLONRT*CONE_FACTOR*pi/180.0

      IF(PHIC < 0.0) ANG=-ANG

      mu = ev*SIN(ANG) + eu*COS(ANG)
      mv = ev*COS(ANG) - eu*SIN(ANG)

END SUBROUTINE earth_2_model_wind

