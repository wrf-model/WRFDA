subroutine da_transpose_z2x (xp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2x(xp % domdesc, xp % comms, xp % xpose_id1)
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_z2x

subroutine da_transpose_x2y (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y(xp % domdesc, xp % comms, xp % xpose_id1)
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_x2y

subroutine da_transpose_x2y_v2(xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y(xp % domdesc, xp % comms, xp % xpose_id2)
#else
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_x2y_v2

subroutine da_transpose_y2z (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2z (xp%domdesc, xp%comms, xp%xpose_id1)
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_y2z

subroutine da_transpose_z2y (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2y (xp%domdesc, xp%comms, xp%xpose_id1)
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_z2y

subroutine da_transpose_y2x (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x(xp % domdesc, xp % comms, xp % xpose_id1)
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_y2x

subroutine da_transpose_y2x_v2(xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x(xp % domdesc, xp % comms, xp % xpose_id2)
#else
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_y2x_v2

subroutine da_transpose_x2z (xp)

   implicit none

   type (xpose_type), intent(inout) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2z(xp % domdesc , xp % comms, xp % xpose_id1)
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

end subroutine da_transpose_x2z


