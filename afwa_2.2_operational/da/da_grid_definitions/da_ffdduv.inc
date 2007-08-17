subroutine da_ffdduv (F,D,U,V,YLON,ID)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   ! When ID =  1
   ! Convert wind speed (F in m/s) and direction (D in degree 0-360) into
   ! wind (U-V in m/s) components
   !
   ! When ID = -1
   ! Convert wind (U-V in m/s) components into wind speed (F in m/s) and 
   ! direction (D in degree 0-360)
   !
   ! Need map projection parameters from module da_control
   !
   ! PHIC:  Central latitude 
   ! XLONC: Central longitude
   ! XN:    Cone projection
   ! CONV:  180/Pi
   !
   !-------------------------------------------------------------------------

   implicit none

   real,    intent (inout) :: F,D
   real,    intent (inout) :: U, V
   real,    intent (in)    :: YLON
   integer, intent (in)    :: ID

   real :: AEARTH, UEARTH, VEARTH
   real :: XLONRT, ANG, conv


   conv = 180.0 / pi

   select case (ID)

      case (convert_fd2uv);

         ! CONVERT WinD module/DIRECTION inTO U/V WinD COMPONENTS ON EARTH,
         ! then CONVERT U/V WinD COMPONENTS ON EARTH inTO LAMBERT CONFORMAL OR
         ! POLAR STEREOGRAPHIC PROJECTION U/V WinD COMPONENTS.

         ! PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE CONSTANT, XN
         ! EQUATIONS REMAin THE SAME.

         AEARTH = D/CONV

         UEARTH = -F*Sin(AEARTH)
         VEARTH = -F*COS(AEARTH)

         ! FOR CONVERSION TO GRID COORDinATES,
         ! SEE program DATAMAP, SUBR VECT, AND
         ! ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

         XLONRT = XLONC-YLON

         if (XLONRT .GT. 180.) XLONRT=XLONRT-360.
         if (XLONRT .LT.-180.) XLONRT=XLONRT+360.

         ANG=XLONRT*CONE_FACTOR/CONV

         ! FOR MERCATOR PROJECTION, THE WinDS ARE AS in EARTH COORDinATES

         if (map_projection.EQ.3) ANG=0.

         if (PHIC.LT.0.0) ANG=-ANG

         U = VEARTH*Sin(ANG) + UEARTH*COS(ANG)
         V = VEARTH*COS(ANG) - UEARTH*Sin(ANG)


         ! CONVERT LAMBERT CONFORMAL OR POLAR STEREOGRAPHIC PROJECTION U/V
         ! WinD COMPONENTS inTO U/V WinD COMPONENTS ON EART
         ! then CONVERT U/V WinD COMPONENTS ON EARTH inTO WinD module/DIRECTION

         ! PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE_FACTOR

      case (convert_uv2fd);

         XLONRT = XLONC-YLON

         if (XLONRT .GT. 180.) XLONRT=XLONRT-360.
         if (XLONRT .LT.-180.) XLONRT=XLONRT+360.

         ANG=XLONRT*CONE_FACTOR/CONV

         ! FOR MERCATOR PROJECTION, THE WinDS ARE AS in EARTH COORDinATES

         if (map_projection .EQ.  3) ANG = 0.
         if (PHIC  .LT. 0.) ANG = -ANG

         UEARTH = U*COS(ANG) - V*Sin(ANG)
         VEARTH = U*Sin(ANG) + V*COS(ANG)

         F = sqrt(UEARTH*UEARTH + VEARTH*VEARTH)

         if (F .EQ. 0.0) then
            D = 0.
            return
         end if

         if (VEARTH .EQ. 0.) then
            if (UEARTH .GT. 0.) D = 270.
            if (UEARTH .LT. 0.) D =  90.
         else
            AEARTH = ATAN (UEARTH/VEARTH)*CONV

            if (UEARTH .LE. 0.0 .AND. VEARTH .LE. 0.0) D = AEARTH
            if (UEARTH .LE. 0.0 .AND. VEARTH .GE. 0.0) D = AEARTH + 180.0
            if (UEARTH .GE. 0.0 .AND. VEARTH .GE. 0.0) D = AEARTH + 180.0
            if (UEARTH .GE. 0.0 .AND. VEARTH .LE. 0.0) D = AEARTH + 360.0

         end if

      case default
         write(unit=message(1),fmt='(A,I2)') ' UNKNOWN OPTION ',ID
         call da_error(__FILE__,__LINE__,message(1:1))

   end select

end subroutine da_ffdduv


