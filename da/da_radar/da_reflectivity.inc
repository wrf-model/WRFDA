subroutine da_reflectivity(rf, rho, qrn)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: rho, qrn
   real, intent(out) :: rf

   real, parameter   :: leh1=43.1
   real, parameter   :: leh2=17.5

   if (trace_use_frequent) call da_trace_entry("da_reflectivity")

   rf = leh1 + leh2 * ALOG10(rho * qrn * 1.0e+3)

   if (trace_use_frequent) call da_trace_exit("da_reflectivity")

end subroutine da_reflectivity


