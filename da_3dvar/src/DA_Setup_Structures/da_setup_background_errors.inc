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

   if ( global ) then
      call da_setup_be_global( ide, jde, kde, be )
   else
      call da_setup_background_errors5( xb, be, xbx, xp )
   endif

   call da_setup_cv( its, ite, jts, jte, be )

end subroutine da_setup_background_errors

