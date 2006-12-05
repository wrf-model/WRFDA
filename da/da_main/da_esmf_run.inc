subroutine da_wrfvar_run( gcomp, importState, exportState, clock, rc )

   type(ESMF_GridComp), intent(inout) :: gcomp
   type(ESMF_State),    intent(inout) :: importState, exportState
   type(ESMF_Clock),    intent(inout) :: clock
   integer,             intent(out)   :: rc

   !<DESCRIPTION>
   !     WRFVAR run routine.
   !
   !     The arguments are:
   !       gcomp           Component
   !       importState     Importstate
   !       exportState     Exportstate
   !       clock           External clock
   !       rc              Return code; equals ESMF_SUCCESS if there are no
   !                       errors, otherwise ESMF_FAILURE.
   !</DESCRIPTION>

   !--do wrfvar

   call da_wrfvar_interface ( head_grid, config_flags )

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_wrfvar_run


