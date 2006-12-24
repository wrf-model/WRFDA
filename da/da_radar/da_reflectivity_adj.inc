subroutine da_reflectivity_adj(rf, rho, qrn, qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(inout)  :: rho, qrn
   real, intent(in)     :: qrn9
   real, intent(inout)  :: rf

   real, parameter   :: leh1=43.1
   real, parameter   :: leh2=17.5

   ! temp = leh2 / (rho9*qrn9*alog(10.))

   ! rho = rho + temp * qrn9 * rf
   ! qrn = qrn + temp * rho9 * rf
   qrn = qrn + leh2/(qrn9*alog(10.)) * rf

end subroutine da_reflectivity_adj


