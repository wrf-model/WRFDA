subroutine da_set_merc(proj)
  
   !--------------------------------------------------------------------------
   ! Purpose: Sets up the remaining basic elements for the mercator projection
   !--------------------------------------------------------------------------

   implicit none

   type(proj_info), intent(inout)       :: proj
   real                                 :: clain

   !  Preliminary variables

   clain = COS(rad_per_deg*proj%truelat1)
   proj%dlon = proj%dx / (earth_radius_m * clain)

   ! Compute distance from equator to origin, and store in the 
   ! proj%rsw tag.

   proj%rsw = 0.0
   if (proj%lat1 .NE. 0.0) then
      proj%rsw = (alog(tan(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
   end if

end subroutine da_set_merc


