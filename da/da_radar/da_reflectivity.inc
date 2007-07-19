subroutine da_reflectivity(rf, rho, qrn)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: rho, qrn
   real, intent(out) :: rf

   real, parameter   :: leh1=43.1
   real, parameter   :: leh2=17.5

   rf = leh1 + leh2 * ALOG10(rho * qrn * 1.0e+3)

end subroutine da_reflectivity


