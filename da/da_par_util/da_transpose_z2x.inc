subroutine da_transpose_z2x (grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_z2x")

#include "XPOSE_V1_z2x.inc"

   if (trace_use) call da_trace_exit("da_transpose_z2x")

end subroutine da_transpose_z2x


