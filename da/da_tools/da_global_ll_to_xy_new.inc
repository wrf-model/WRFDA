subroutine da_global_ll_to_xy_new(lat,lon,x,y)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the(x,y) location(dot) in the global grids
   !           from latitudes and longitudes
   !----------------------------------------------------------------------------
   
   implicit none
   
   real, intent(in)  :: lat(:,:)
   real, intent(in)  :: lon(:,:)
   real, intent(out) :: x(:,:)
   real, intent(out) :: y(:,:)

real :: xlat, xlon
integer :: a,b

   if (trace_use) call da_trace_entry("da_global_ll_to_xy_new")

!FAST

!   where (lat(:,:) - start_lat < 0)
!      y(:,:) = start_y + (lat(:,:) - start_lat+180.0)/delt_lat
!   else
!      y(:,:) = start_y + (lat(:,:) - start_lat)/delt_lat
!   end where

!   where (lon(:,:) - start_lon < 0.)
!      x(:,:) = start_x + (lon(:,:) - start_lon+360.0)/delt_lon
!   else
!      x(:,:) = start_x + (lon(:,:) - start_lon)/delt_lon
!   end where

! SLOW

do a=lbound(lat,1),ubound(lat,1)
do b=lbound(lat,2),ubound(lat,2)
   
   xlat = lat(a,b) - start_lat
   xlon = lon(a,b) - start_lon

   if (xlat < 0.0) xlat = xlat + 180.0
   if (xlon < 0.0) xlon = xlon + 360.0

   x(a,b) = start_x + xlon/delt_lon
   y(a,b) = start_y + xlat/delt_lat

end do
end do

   if (trace_use) call da_trace_exit("da_global_ll_to_xy_new")
   
end subroutine da_global_ll_to_xy_new


