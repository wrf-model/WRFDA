SUBROUTINE da_reflectivity_Adj( rf, rho, qrn, rho9, qrn9 )

   implicit none

   real, intent(inout)  :: rho, qrn
   real, intent(in)     :: rho9, qrn9
   real, intent(inout)  :: rf

   REAL, PARAMETER   :: leh1=43.1
   REAL, PARAMETER   :: leh2=17.5

   REAL   :: temp


   temp = leh2 / ( rho9*qrn9*alog(10.) )

   rho = rho + temp * qrn9 * rf
   qrn = qrn + temp * rho9 * rf

END SUBROUTINE da_reflectivity_Adj
