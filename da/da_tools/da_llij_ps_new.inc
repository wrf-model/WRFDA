subroutine da_llij_ps_new(lat,lon,proj,i,j)

   !-----------------------------------------------------------------------
   ! Purpose: Given latitude (-90 to 90), longitude (-180 to 180), and the
   ! standard polar-stereographic projection information via the 
   ! public proj structure, this routine returns the i/j indices which
   ! if within the domain range from 1->nx and 1->ny, respectively.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)               :: lat(:,:)
   real, intent(in)               :: lon(:,:)
   type(proj_info),intent(in)     :: proj

   real, intent(out)              :: i(:,:) !(x-index)
   real, intent(out)              :: j(:,:) !(y-index)
   
   real                           :: reflon

integer :: a,b
real :: scale_top, ala, rm, alo

   if (trace_use) call da_trace_entry("da_llij_ps_new")

   reflon = proj%stdlon + 90.0

   ! FAST

!   i(:,:) = proj%polei + proj%rebydx * COS(lat(:,:) * rad_per_deg) &
!      * (1.0 + proj%hemi * SIN(proj%truelat1 * rad_per_deg) &
!      / (1.0 + proj%hemi *SIN(lat(:,:) * rad_per_deg)) &
!      * COS((lon(:,:) - reflon) * rad_per_deg)

!   j(:,:) = proj%polej + proj%hemi * proj%rebydx * COS(lat(:,:) * rad_per_deg) &
!      * (1.0 + proj%hemi * SIN(proj%truelat1 * rad_per_deg) &
!      / (1.0 + proj%hemi *SIN(lat(:,:) * rad_per_deg)) &
!      * SIN((lon(:,:) - reflon) * rad_per_deg)

! SLOW

do a=lbound(lat,1),ubound(lat,1)
do b=lbound(lat,2),ubound(lat,2)
   ! Compute numerator term of map scale factor

   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Find radius to desired point
   ala = lat(a,b) * rad_per_deg
   rm = proj%rebydx * COS(ala) * scale_top/(1.0 + proj%hemi *Sin(ala))
   alo = (lon(a,b) - reflon) * rad_per_deg
   i(a,b) = proj%polei + rm * COS(alo)
   j(a,b) = proj%polej + proj%hemi * rm * Sin(alo)
end do
end do

   if (trace_use) call da_trace_exit("da_llij_ps_new")
 
end subroutine da_llij_ps_new


