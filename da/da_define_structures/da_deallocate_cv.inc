subroutine da_deallocate_cv( cv )

   !---------------------------------------------------------------------------
   ! PURPOSE: Allocate components of control variable.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (cv_type), INTENT(INOUT)        :: cv     ! Control variable structure.

   IF (trace_use) call da_trace_entry("da_deallocate_cv")
  
   !---------------------------------------------------------------------------
   ! [1.0] Deallocate arrays:
   !---------------------------------------------------------------------------

   DEALLOCATE ( cv % array )

   IF (trace_use) call da_trace_exit("da_deallocate_cv")
   
end subroutine da_deallocate_cv

