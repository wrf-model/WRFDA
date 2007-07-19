subroutine da_llij_ps(lat,lon,proj,i,j)

   !-----------------------------------------------------------------------
   ! Purpose: Given latitude (-90 to 90), longitude (-180 to 180), and the
   ! standard polar-stereographic projection information via the 
   ! public proj structure, this routine returns the i/j indices which
   ! if within the domain range from 1->nx and 1->ny, respectively.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)               :: lat
   real, intent(in)               :: lon
   type(proj_info),intent(in)     :: proj

   real, intent(out)              :: i !(x-index)
   real, intent(out)              :: j !(y-index)
   
   real                           :: reflon
   real                           :: scale_top
   real                           :: ala
   real                           :: alo
   real                           :: rm

   reflon = proj%stdlon + 90.0
   
   ! Compute numerator term of map scale factor

   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Find radius to desired point
   ala = lat * rad_per_deg
   rm = proj%rebydx * COS(ala) * scale_top/(1.0 + proj%hemi *Sin(ala))
   alo = (lon - reflon) * rad_per_deg
   i = proj%polei + rm * COS(alo)
   j = proj%polej + proj%hemi * rm * Sin(alo)
 
end subroutine da_llij_ps


