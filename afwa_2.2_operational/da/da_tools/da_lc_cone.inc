subroutine da_lc_cone(truelat1, truelat2, cone)

   !------------------------------------------------------------------------
   ! Purpose: compute the cone factor of a Lambert Conformal projection
   !------------------------------------------------------------------------

   implicit none
    
   real, intent(in)             :: truelat1  ! (-90 -> 90 degrees N)
   real, intent(in)             :: truelat2  !   "   "  "   "     "
   real, intent(out)            :: cone

   ! First, see if this is a secant or tangent projection.  For tangent
   ! projections, truelat1 = truelat2 and the cone is tangent to the 
   ! Earth's surface at this latitude.  For secant projections, the cone
   ! intersects the Earth's surface at each of the distinctly different
   ! latitudes
   if (ABS(truelat1-truelat2) > 0.1) then
      cone = ALOG10(COS(truelat1*rad_per_deg)) - &
             ALOG10(COS(truelat2*rad_per_deg))
      cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
             ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
   else
      cone = Sin(ABS(truelat1)*rad_per_deg)  
   end if

end subroutine da_lc_cone


