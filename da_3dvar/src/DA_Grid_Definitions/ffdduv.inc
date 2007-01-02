SUBROUTINE FFDDUV (F,D,U,V,YLON,ID)
!------------------------------------------------------------------------------!
! When ID =  1
! Convert wind speed (F in m/s) and direction (D in degree 0-360) into MM5
! wind (U-V in m/s) components
!
! When ID = -1
! Convert MM5 wind (U-V in m/s) components into wind speed (F in m/s) and 
! direction (D in degree 0-360)
!
! Need MM5 map projection parameters from module modulke_map.F
!
! IPROJ: Projection type
! PHIC:  Central latitude 
! XLONC: Central longitude
! XN:    Cone projection
! CONV:  180/Pi
!
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL,    INTENT (inout) :: F,D
    REAL,    INTENT (inout) :: U, V
    REAL,    INTENT (in)    :: YLON
    INTEGER, INTENT (in)    :: ID

    REAL :: AEARTH, UEARTH, VEARTH
    REAL :: XLONRT, ANG, conv
!------------------------------------------------------------------------------!
    conv = 180.0 / pi

    SELECT CASE (ID)

      CASE (1);

!     CONVERT WIND MODULE/DIRECTION INTO U/V WIND COMPONENTS ON EARTH,
!     THEN CONVERT U/V WIND COMPONENTS ON EARTH INTO LAMBERT CONFORMAL OR
!     POLAR STEREOGRAPHIC PROJECTION U/V WIND COMPONENTS.
!
!     PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE CONSTANT, XN
!     EQUATIONS REMAIN THE SAME.

      AEARTH = D/CONV

      UEARTH = -F*SIN(AEARTH)
      VEARTH = -F*COS(AEARTH)

!     FOR CONVERSION TO GRID COORDINATES,
!     SEE PROGRAM DATAMAP, SUBR VECT, AND
!     ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

      XLONRT = XLONC-YLON

      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.

      ANG=XLONRT*CONE_FACTOR/CONV

!   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES

      IF(map_projection.EQ.3) ANG=0.

      IF(PHIC.LT.0.0) ANG=-ANG

      U = VEARTH*SIN(ANG) + UEARTH*COS(ANG)
      V = VEARTH*COS(ANG) - UEARTH*SIN(ANG)


!     CONVERT LAMBERT CONFORMAL OR POLAR STEREOGRAPHIC PROJECTION U/V
!     WIND COMPONENTS INTO U/V WIND COMPONENTS ON EART
!     THEN CONVERT U/V WIND COMPONENTS ON EARTH INTO WIND MODULE/DIRECTION
!
!     PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE_FACTOR

      CASE (-1);

      XLONRT = XLONC-YLON

      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.

      ANG=XLONRT*CONE_FACTOR/CONV

!
!   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES
!
      IF(map_projection .EQ.  3) ANG = 0.
      IF(PHIC  .LT. 0.) ANG = -ANG

      UEARTH = U*COS(ANG) - V*SIN(ANG)
      VEARTH = U*SIN(ANG) + V*COS(ANG)

      F = SQRT(UEARTH*UEARTH + VEARTH*VEARTH)

      IF (F .EQ. 0.0) THEN
         D = 0.
         RETURN
      ENDIF

      IF (VEARTH .EQ. 0.) THEN

         IF (UEARTH .GT. 0.) D = 270.
         IF (UEARTH .LT. 0.) D =  90.

      ELSE

         AEARTH = ATAN (UEARTH/VEARTH)*CONV

         IF (UEARTH .LE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH
         IF (UEARTH .LE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH + 360.0

      ENDIF

      CASE DEFAULT

           WRITE (0,'(/,A,I2,/)') ' UNKNOWN OPTION ',ID
           STOP ' in ffdduv.F'

    END SELECT

END SUBROUTINE FFDDUV
