subroutine da_llij_merc(lat, lon, proj, i, j)

   !-----------------------------------------------------------------------
   ! Purpose: Compute i/j coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   real, intent(in)              :: lat
   real, intent(in)              :: lon
   type(proj_info),intent(in)    :: proj
   real,intent(out)              :: i
   real,intent(out)              :: j
   real                          :: deltalon

   deltalon = lon - proj%lon1
   if (deltalon < -180.0) deltalon = deltalon + 360.0
   if (deltalon > 180.0) deltalon = deltalon - 360.0
   i = 1. + (deltalon/(proj%dlon*deg_per_rad))
   j = 1. + (ALOG(TAN(0.5*((lat + 90.0) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

end subroutine da_llij_merc


