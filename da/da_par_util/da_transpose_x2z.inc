subroutine da_transpose_x2z (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_x2z")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2z(xp % domdesc , xp % comms, XPOSE_V1)
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_x2z")

end subroutine da_transpose_x2z


