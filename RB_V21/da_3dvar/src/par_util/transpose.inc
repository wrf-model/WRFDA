SUBROUTINE transpose_z2x ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2x( xp % domdesc, xp % comms, xp % xpose_id1 )
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_z2x

SUBROUTINE transpose_x2y ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y( xp % domdesc, xp % comms, xp % xpose_id1 )
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_x2y

SUBROUTINE transpose_x2y_v2( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2y( xp % domdesc, xp % comms, xp % xpose_id2 )
#else
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_x2y_v2

SUBROUTINE transpose_y2z ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2z ( xp%domdesc, xp%comms, xp%xpose_id1 )
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_y2z

SUBROUTINE transpose_z2y ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_z2y ( xp%domdesc, xp%comms, xp%xpose_id1 )
#else
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_z2y

SUBROUTINE transpose_y2x ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x( xp % domdesc, xp % comms, xp % xpose_id1 )
#else
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_y2x

SUBROUTINE transpose_y2x_v2( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_y2x( xp % domdesc, xp % comms, xp % xpose_id2 )
#else
   xp % v2x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v2y(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_y2x_v2

SUBROUTINE transpose_x2z ( xp )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(INOUT) :: xp      ! Dimensions and xpose buffers.

#ifdef DM_PARALLEL
   call wrf_dm_xpose_x2z( xp % domdesc , xp % comms, xp % xpose_id1 )
#else
   xp % v1z(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde) = &
   xp % v1x(xp%ids:xp%ide,xp%jds:xp%jde,xp%kds:xp%kde)
#endif

END SUBROUTINE transpose_x2z

#ifndef DM_PARALLEL
SUBROUTINE wrf_dm_halo ( domdesc , comms , stencil_id )
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , stencil_id
   RETURN
END SUBROUTINE wrf_dm_halo
#endif

