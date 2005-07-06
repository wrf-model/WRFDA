SUBROUTINE da_reflectivity_Lin( rf, rho, qrn, rho9, qrn9 )

   implicit none

   real, intent(in)  :: rho, qrn
   real, intent(in)  :: rho9, qrn9
   real, intent(out) :: rf

   REAL, PARAMETER   :: leh1=43.1
   REAL, PARAMETER   :: leh2=17.5

   rf = leh2 * qrn /(qrn9*alog(10.)) 

END SUBROUTINE da_reflectivity_Lin
