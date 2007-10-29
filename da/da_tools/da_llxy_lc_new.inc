subroutine da_llxy_lc_new(lat, lon, proj, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: compute the geographical latitude and longitude values
   ! to the cartesian x/y on a Lambert Conformal projection.
   !-----------------------------------------------------------------------
    
   implicit none

   real, intent(in)              :: lat(:,:)      ! Latitude (-90->90 deg N)
   real, intent(in)              :: lon(:,:)      ! Longitude (-180->180 E)
   type(proj_info),intent(in)      :: proj     ! Projection info structure

   real, intent(out)             :: x(:,:)        ! Cartesian X coordinate
   real, intent(out)             :: y(:,:)        ! Cartesian Y coordinate


   real    :: tl1r
   real    :: temp1
   real    :: temp2
   real    :: ctl1r
   real    :: deltalon, rm, arg

   integer :: a,b

   if (trace_use) call da_trace_entry("da_llxy_lc_new")

! FAST
    
   ! Convert truelat1 to radian and compute COS for later use
!   tl1r = proj%truelat1 * rad_per_deg
!   ctl1r = COS(tl1r)    
!   temp1 = TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0)
!   temp2 = proj%rebydx * ctl1r/proj%cone
    
   ! Compute deltalon between known longitude and standard lon and ensure
   ! it is not in the cut zone

!   where (lon - proj%stdlon > +180.0)
!      x = proj%polei + proj%hemi * (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1)**proj%cone &
!         * SIN(proj%cone*((lon - proj%stdlon-360.0)*rad_per_deg))
!      y = proj%polej - (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1)**proj%cone &
!         * COS(proj%cone*((lon - proj%stdlon-360.0)*rad_per_deg))
!   elsewhere  (lon - proj%stdlon - -180.0)
!      x = proj%polei + proj%hemi * (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1) &
!         * SIN(proj%cone*((lon - proj%stdlon+360.0)*rad_per_deg))
!      y = proj%polej - (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1)**proj%cone &
!         * COS(proj%cone*((lon - proj%stdlon+360.0)*rad_per_deg))
!   else
!      x = proj%polei + proj%hemi * (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1) &
!         * SIN(proj%cone*((lon - proj%stdlon)*rad_per_deg))
!      y = proj%polej - (temp2 * (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / temp1)**proj%cone &
!         * COS(proj%cone*((lon - proj%stdlon)*rad_per_deg))
!   end where

   ! Finally, if we are in the southern hemisphere, flip the i/j
   ! values to a coordinate system where (1,1) is the SW corner
   ! (what we assume) which is different than the original NCEP
   ! algorithms which used the NE corner as the origin in the 
   ! southern hemisphere (left-hand vs. right-hand coordinate?)
!   if (proj%hemi == -1.0) then
!      x(:,:) = 2.0 - i(:,:)
!      y(:,:) = 2.0 - j(:,:)
!   end if

! SLOW

   do a=lbound(lat,1), ubound(lat,1)
      do b=lbound(lat,2), ubound(lat,2)

         ! Compute deltalon between known longitude and standard lon and ensure
         ! it is not in the cut zone
         deltalon = lon(a,b) - proj%stdlon
         if (deltalon > +180.0) deltalon = deltalon - 360.0
         if (deltalon < -180.0) deltalon = deltalon + 360.0

         ! Convert truelat1 to radian and compute COS for later use
         tl1r = proj%truelat1 * rad_per_deg
         ctl1r = COS(tl1r)     

         ! Radius to desired point
         rm = proj%rebydx * ctl1r/proj%cone * &
             (TAN((90.0*proj%hemi-lat(a,b))*rad_per_deg/2.0) / &
              TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

         arg = proj%cone*(deltalon*rad_per_deg)
         x(a,b) = proj%polei + proj%hemi * rm * Sin(arg)
         y(a,b) = proj%polej - rm * COS(arg)

         ! Finally, if we are in the southern hemisphere, flip the i/j
         ! values to a coordinate system where (1,1) is the SW corner
         ! (what we assume) which is different than the original NCEP
         ! algorithms which used the NE corner as the origin in the 
         ! southern hemisphere (left-hand vs. right-hand coordinate?)
         if (proj%hemi == -1.0) then
            x(a,b) = 2.0 - x(a,b)  
            y(a,b) = 2.0 - y(a,b)
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_llxy_lc_new")

end subroutine da_llxy_lc_new


