SUBROUTINE da_reflectivity( rf, rho, qrn )

   implicit none

   real, intent(in)  :: rho, qrn
   real, intent(out) :: rf

   REAL, PARAMETER   :: leh1=43.1
   REAL, PARAMETER   :: leh2=17.5

   rf = leh1 + leh2 * ALOG10( rho * qrn * 1.E+3 )

END SUBROUTINE da_reflectivity
