subroutine da_llij_latlon_new(lat, lon, proj, i, j)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the i/j location of a lat/lon on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat(:,:)
   real, intent(in)             :: lon(:,:)
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: i(:,:)
   real, intent(out)            :: j(:,:)

   if (trace_use) call da_trace_entry("da_llij_latlon_new")

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   ! Compute the difference between the input lat/lon and the origin lat/lon

   j(:,:) = (lat(:,:) - proj%lat1)/proj%truelat1 + 1.0

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   where (lon(:,:) < 0)
      i(:,:) = (lon(:,:) + 360.0  - proj%lon1)/proj%stdlon + 1.0
   elsewhere
      i(:,:) = (lon(:,:) - proj%lon1)/proj%stdlon + 1.0
   end where

   if (trace_use) call da_trace_exit("da_llij_latlon_new")

end subroutine da_llij_latlon_new


