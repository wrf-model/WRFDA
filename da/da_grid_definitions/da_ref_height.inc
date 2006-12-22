subroutine da_ref_height(pres, ref_height)

   !---------------------------------------------------------------------------
   ! PURPOSE: To calculate the height from the reference pressure
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: pres
   real, intent(out) :: ref_height

   real              :: aa, bb, cc
   real              :: p0iso, ziso

   real, parameter   :: rovg = gas_constant/gravity

   aa = 0.5 * rovg * tlp
   bb = rovg * ts0

   cc = log(pres/ps0)
   ref_height = -(bb + aa * cc) * cc

   if (tis0 > 0.0) then
      p0iso=ps0*exp( (tis0-ts0)/tlp )
      cc=log(p0iso/ps0)
      ziso=-(aa*cc+bb)*cc

      if(ref_height > ziso) then
         ref_height=ziso+rovg*tis0*log(p0iso/pres)
      end if
   end if

end subroutine da_ref_height


