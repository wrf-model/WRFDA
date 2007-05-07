subroutine da_transpose_y2x_v2(xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_y2x_v2")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x(xp % domdesc, xp % comms, XPOSE_V2)
#else
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_y2x_v2")

end subroutine da_transpose_y2x_v2


