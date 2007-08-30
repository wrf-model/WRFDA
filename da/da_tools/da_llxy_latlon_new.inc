subroutine da_llxy_latlon_new(lat, lon, proj, ix, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat(:,:)
   real, intent(in)             :: lon(:,:)
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: x(:,:)
   real, intent(out)            :: y(:,:)

   if (trace_use) call da_trace_entry("da_llxy_latlon_new")

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   ! Compute the difference between the input lat/lon and the origin lat/lon

   y(:,:) = (lat(:,:) - proj%lat1)/proj%truelat1 + 1.0

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   where (lon(:,:) < 0)
      x(:,:) = (lon(:,:) + 360.0  - proj%lon1)/proj%stdlon + 1.0
   elsewhere
      x(:,:) = (lon(:,:) - proj%lon1)/proj%stdlon + 1.0
   end where

   if (trace_use) call da_trace_exit("da_llxy_latlon_new")

end subroutine da_llxy_latlon_new


