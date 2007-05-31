subroutine da_transpose_y2x (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_y2x")

#include "XPOSE_V1_y2x.inc"

   if (trace_use) call da_trace_exit("da_transpose_y2x")

end subroutine da_transpose_y2x


