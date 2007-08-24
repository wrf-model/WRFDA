subroutine da_reflectivity_adj(rf, qrn, qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(inout)  :: qrn
   real, intent(in)     :: qrn9
   real, intent(inout)  :: rf

   real, parameter   :: leh1=43.1
   real, parameter   :: leh2=17.5

   if (trace_use_frequent) call da_trace_entry("da_reflectivity_adj")

   ! temp = leh2 / (rho9*qrn9*alog(10.0))

   ! rho = rho + temp * qrn9 * rf
   ! qrn = qrn + temp * rho9 * rf
   qrn = qrn + leh2/(qrn9*alog(10.0)) * rf

   if (trace_use_frequent) call da_trace_exit("da_reflectivity_adj")

end subroutine da_reflectivity_adj


