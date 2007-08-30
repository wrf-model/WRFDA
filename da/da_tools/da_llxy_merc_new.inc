subroutine da_llxy_merc_new(lat, lon, proj, x,y)

   !-----------------------------------------------------------------------
   ! Purpose: Compute x/y coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   real, intent(in)              :: lat(:,:)
   real, intent(in)              :: lon(:,:)
   type(proj_info),intent(in)    :: proj
   real,intent(out)              :: x(:,:)
   real,intent(out)              :: y(:,:)

real :: deltalon
integer :: a,b

if (trace_use) call da_trace_entry("da_llxy_merc_new")

! FAST

!   where (lon(:,:) - proj%lon1 < -180.0)
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1 + 360.0)/(proj%dlon*deg_per_rad))
!   elsewhere (lon(:,:) - proj%lon1 > 180.0)
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1 - 360.0)/(proj%dlon*deg_per_rad))
!   else
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1)/(proj%dlon*deg_per_rad))
!   end where

!   y(:,:) = 1.0 + (ALOG(TAN(0.5*((lat(:,: + 90.0) * rad_per_deg)))) / proj%dlon - proj%rsw

! SLOW

do a=lbound(lat,1),ubound(lat,1)
do b=lbound(lat,2),ubound(lat,2)
   deltalon = lon(a,b) - proj%lon1
   if (deltalon < -180.0) deltalon = deltalon + 360.0
   if (deltalon > 180.0) deltalon = deltalon - 360.0
   x(a,b) = 1.0 + (deltalon/(proj%dlon*deg_per_rad))
   y(a,b) = 1.0 + (ALOG(TAN(0.5*((lat(a,b) + 90.0) * rad_per_deg)))) / &
           proj%dlon - proj%rsw
end do
end do

if (trace_use) call da_trace_exit("da_llxy_merc_new")


end subroutine da_llxy_merc_new


