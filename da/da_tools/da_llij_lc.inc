subroutine da_llij_lc(lat, lon, proj, i, j)

   !-----------------------------------------------------------------------
   ! Purpose: compute the geographical latitude and longitude values
   ! to the cartesian x/y on a Lambert Conformal projection.
   !-----------------------------------------------------------------------
    
   implicit none

   real, intent(in)              :: lat      ! Latitude (-90->90 deg N)
   real, intent(in)              :: lon      ! Longitude (-180->180 E)
   type(proj_info),intent(in)      :: proj     ! Projection info structure

   real, intent(out)             :: i        ! Cartesian X coordinate
   real, intent(out)             :: j        ! Cartesian Y coordinate

   real                          :: arg
   real                          :: deltalon
   real                          :: tl1r
   real                          :: rm
   real                          :: ctl1r

   if (trace_use_dull) call da_trace_entry("da_llij_lc")
    
   ! Compute deltalon between known longitude and standard lon and ensure
   ! it is not in the cut zone
   deltalon = lon - proj%stdlon
   if (deltalon > +180.0) deltalon = deltalon - 360.0
   if (deltalon < -180.0) deltalon = deltalon + 360.0
    
   ! Convert truelat1 to radian and compute COS for later use
   tl1r = proj%truelat1 * rad_per_deg
   ctl1r = COS(tl1r)     
   
   ! Radius to desired point
   rm = proj%rebydx * ctl1r/proj%cone * &
       (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / &
        TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

   arg = proj%cone*(deltalon*rad_per_deg)
   i = proj%polei + proj%hemi * rm * Sin(arg)
   j = proj%polej - rm * COS(arg)

   ! Finally, if we are in the southern hemisphere, flip the i/j
   ! values to a coordinate system where (1,1) is the SW corner
   ! (what we assume) which is different than the original NCEP
   ! algorithms which used the NE corner as the origin in the 
   ! southern hemisphere (left-hand vs. right-hand coordinate?)
   if (proj%hemi == -1.0) then
      i = 2.0 - i  
      j = 2.0 - j
   end if

   if (trace_use_dull) call da_trace_exit("da_llij_lc")

end subroutine da_llij_lc


