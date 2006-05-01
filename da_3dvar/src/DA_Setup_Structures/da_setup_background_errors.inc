subroutine da_setup_background_errors( xb, xbx, be, xp, &
                                       its, ite, jts, jte, kts, kte, &
                                       ids, ide, jds, jde, kds, kde )

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!           Wrapper subroutine.
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (xbx_type),INTENT(IN)  :: xbx                   ! Header & non-gridded vars.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.
   type (xpose_type), intent(inout) :: xp               ! Dimensions and xpose buffers.
   integer, intent(in)         :: its, ite, jts, jte, kts, kte ! Tile dimensions.
   integer, intent(in)         :: ids, ide, jds, jde, kds, kde ! Tile dimensions.

   IF (trace_use) CALL da_trace_entry("da_setup_background_errors")

   be % ne = ensdim_alpha                          ! Size of ensemble.

   if ( global ) then
      call da_setup_be_global( ide, jde, kde, be )
   else
      call da_setup_be_regional( xb, be, xbx, xp )
   endif

   call da_setup_cv( its, ite, jts, jte, &
                     ids, ide, jds, jde, be )

   IF (trace_use) CALL da_trace_exit("da_setup_background_errors")

end subroutine da_setup_background_errors

