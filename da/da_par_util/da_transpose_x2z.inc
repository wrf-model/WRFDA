subroutine da_transpose_x2z (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_x2z")

#include "XPOSE_V1_x2z.inc"

   if (trace_use) call da_trace_exit("da_transpose_x2z")

end subroutine da_transpose_x2z


