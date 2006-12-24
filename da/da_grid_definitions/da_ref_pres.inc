subroutine da_ref_pres(height, ref_pres)

   !---------------------------------------------------------------------------
   ! PURPOSE: To calculate the reference pressure from the height.
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: height
   real, intent(out) :: ref_pres

   real, parameter :: rovg = gas_constant/gravity

   real :: aa, bb, cc, hh, htop, ziso, p0iso

   call da_ref_height(ptop, htop)

   bb = rovg * ts0
   aa = rovg * tlp * 0.5

   hh = min(htop, height)
   cc = (-bb + sqrt(bb * bb - 4.0 * aa * hh))/(2.0*aa)
   ref_pres = ps0 * exp(cc)

   if (tis0 > 0.0) then
      p0iso=ps0*exp((tis0-ts0)/tlp)
      cc=log(p0iso/ps0)
      ziso=-(aa*cc+bb)*cc

      if (height > ziso) then
         ref_pres = p0iso/exp((height-ziso)/(rovg*tis0))
      end if
   end if

end subroutine da_ref_pres


