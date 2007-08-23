subroutine da_ijll_merc(i, j, proj, lat, lon)

   !-----------------------------------------------------------------------
   ! Compute the lat/lon from i/j for mercator projection
   !-----------------------------------------------------------------------

   implicit none

   real,intent(in)               :: i
   real,intent(in)               :: j    
   type(proj_info),intent(in)    :: proj
   real, intent(out)             :: lat
   real, intent(out)             :: lon 

   if (trace_use_frequent) call da_trace_entry("da_ijll_merc")

   lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-1.0)))*deg_per_rad - 90.0
   lon = (i-1.0)*proj%dlon*deg_per_rad + proj%lon1
   if (lon > 180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

   if (trace_use_frequent) call da_trace_exit("da_ijll_merc")

end subroutine da_ijll_merc


