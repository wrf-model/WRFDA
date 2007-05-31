subroutine da_transpose_x2y (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_x2y")

#include "XPOSE_V1_x2y.inc"

   if (trace_use) call da_trace_exit("da_transpose_x2y")

end subroutine da_transpose_x2y


