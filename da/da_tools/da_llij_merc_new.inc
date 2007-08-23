subroutine da_llij_merc_new(lat, lon, proj, i, j)

   !-----------------------------------------------------------------------
   ! Purpose: Compute i/j coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   real, intent(in)              :: lat(:,:)
   real, intent(in)              :: lon(:,:)
   type(proj_info),intent(in)    :: proj
   real,intent(out)              :: i(:,:)
   real,intent(out)              :: j(:,:)

real :: deltalon
integer :: a,b

if (trace_use) call da_trace_entry("da_llij_merc_new")

! FAST

!   where (lon(:,:) - proj%lon1 < -180.0)
!      i(:,:) = 1.0 + (lon(:,:) - proj%lon1 + 360.0)/(proj%dlon*deg_per_rad))
!   elsewhere (lon(:,:) - proj%lon1 > 180.0)
!      i(:,:) = 1.0 + (lon(:,:) - proj%lon1 - 360.0)/(proj%dlon*deg_per_rad))
!   else
!      i(:,:) = 1.0 + (lon(:,:) - proj%lon1)/(proj%dlon*deg_per_rad))
!   end where

!   j(:,:) = 1.0 + (ALOG(TAN(0.5*((lat(:,: + 90.0) * rad_per_deg)))) / proj%dlon - proj%rsw

! SLOW

do a=lbound(lat,1),ubound(lat,1)
do b=lbound(lat,2),ubound(lat,2)
   deltalon = lon(a,b) - proj%lon1
   if (deltalon < -180.0) deltalon = deltalon + 360.0
   if (deltalon > 180.0) deltalon = deltalon - 360.0
   i(a,b) = 1.0 + (deltalon/(proj%dlon*deg_per_rad))
   j(a,b) = 1.0 + (ALOG(TAN(0.5*((lat(a,b) + 90.0) * rad_per_deg)))) / &
           proj%dlon - proj%rsw
end do
end do

if (trace_use) call da_trace_exit("da_llij_merc_new")


end subroutine da_llij_merc_new


