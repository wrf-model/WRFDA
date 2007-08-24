subroutine da_reflectivity_lin(rf, qrn, qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: qrn
   real, intent(in)  :: qrn9
   real, intent(out) :: rf

   real, parameter :: leh1=43.1
   real, parameter :: leh2=17.5

   if (trace_use_frequent) call da_trace_entry("da_reflectivity_lin")

   rf = leh2 * qrn /(qrn9*alog(10.0)) 

   if (trace_use_frequent) call da_trace_exit("da_reflectivity_lin")

end subroutine da_reflectivity_lin


