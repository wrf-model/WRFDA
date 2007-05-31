subroutine da_transpose_x2y_v2 (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_x2y_v2")

#include "XPOSE_V2_x2y.inc"

   if (trace_use) call da_trace_exit("da_transpose_x2y_v2")

end subroutine da_transpose_x2y_v2


