SUBROUTINE DA_Setup_Background_Errors( xb, xbx, be, xp )

!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors.
!           Wrapper subroutine.
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)  :: xb                    ! First guess structure.
   TYPE (xbx_type),INTENT(IN)  :: xbx                   ! Header & non-gridded vars.
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.
   type (xpose_type), intent(inout) :: xp               ! Dimensions and xpose buffers.

   TYPE (be_type)              :: be_g        ! be on globally-dimensioned grid

   integer                     :: jts, jte, kts, kte

   CHARACTER (LEN=120)         :: message

   if(cv_options == 2)then
      CALL DA_Setup_Background_Errors2( xb, xbx, be_g )

!-----Create local be arrays and deallocate global copies.

      jts = xp%jts
      jte = xp%jte
      kts = xp%kts
      kte = xp%kte

      call be_local_copy( be_g, be, jts, jte, kts, kte )

   else if(cv_options == 3)then

      CALL DA_Setup_Background_Errors3( xb,be,xp )

   else

      write(unit=*, fmt='(a,i8/a)') &
           'Invalid cv_options =', cv_options, &
           "Program stopped."

      WRITE( message , * ) &
           'da_3dvar/src/DA_Setup_Structures/da_setup_background_errors.inc: ', &
           'Invalid cv_options =', cv_options
      CALL WRF_ERROR_FATAL ( message )
   endif

END SUBROUTINE DA_Setup_Background_Errors

