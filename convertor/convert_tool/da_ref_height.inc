subroutine DA_Ref_height(pres, ref_height)

!------------------------------------------------------------------------------
!  PURPOSE: To calculate the height from the reference pressure
!
!  METHOD:  MM5 tutorial notes
!
!  HISTORY: 05/11/00                                    Yong-Run Guo
!
!  MODIFIED:	09/26/02				Wei Huang
!------------------------------------------------------------------------------

  IMPLICIT NONE

  real, intent(in)  :: pres
  real, intent(out) :: ref_height

  real              :: aa, bb, cc
  real              :: p0iso, ziso

  real, parameter   :: rovg = gas_constant/gravity

  aa = 0.5 * rovg * tlp
  bb = rovg * ts0

  cc = log(pres/ps0)
  ref_height = -(bb + aa * cc) * cc

  if(tis0 > 0.0) then
    p0iso=ps0*exp( (tis0-ts0)/tlp )
    cc=log(p0iso/ps0)
    ziso=-(aa*cc+bb)*cc

    if(ref_height > ziso) then
       ref_height=ziso+rovg*tis0*log(p0iso/pres)
    endif
  endif

end subroutine DA_Ref_height

