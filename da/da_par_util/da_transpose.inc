subroutine da_transpose_z2x (xp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_z2x")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2x(xp % domdesc, xp % comms, XPOSE_V1)
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_z2x")

end subroutine da_transpose_z2x

subroutine da_transpose_x2y (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_x2y")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y(xp % domdesc, xp % comms, XPOSE_V1)
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_x2y")

end subroutine da_transpose_x2y

subroutine da_transpose_x2y_v2(xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_x2y_v2")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y(xp % domdesc, xp % comms, XPOSE_V2)
#else
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_x2y_v2")

end subroutine da_transpose_x2y_v2

subroutine da_transpose_y2z (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_y2z")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2z (xp%domdesc, xp%comms, XPOSE_V1)
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_y2z")

end subroutine da_transpose_y2z

subroutine da_transpose_z2y (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_z2y")


#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2y (xp%domdesc, xp%comms, XPOSE_V1)
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_z2y")

end subroutine da_transpose_z2y

subroutine da_transpose_y2x (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

   if (trace_use) call da_trace_entry("da_transpose_y2x")

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x(xp % domdesc, xp % comms, XPOSE_V1)
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_y2x")

end subroutine da_transpose_y2x

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


