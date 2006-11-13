subroutine da_deallocate_cv( cv )

   !---------------------------------------------------------------------------
   ! PURPOSE: Allocate components of control variable.
   !---------------------------------------------------------------------------

   implicit none

   type (cv_type), intent(inout)        :: cv     ! Control variable structure.

   if (trace_use) call da_trace_entry("da_deallocate_cv")
  
   !---------------------------------------------------------------------------
   ! [1.0] Deallocate arrays:
   !---------------------------------------------------------------------------

   deallocate ( cv % array )

   if (trace_use) call da_trace_exit("da_deallocate_cv")
   
end subroutine da_deallocate_cv

