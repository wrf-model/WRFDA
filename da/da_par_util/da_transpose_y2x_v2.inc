subroutine da_transpose_y2x_v2 (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_y2x_v2")

#include "XPOSE_V2_y2x.inc"

   if (trace_use) call da_trace_exit("da_transpose_y2x_v2")

end subroutine da_transpose_y2x_v2


