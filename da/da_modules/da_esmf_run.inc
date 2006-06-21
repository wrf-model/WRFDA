subroutine da_wrfvar_run( gcomp, importState, exportState, clock, rc )

   TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
   TYPE(ESMF_State),    INTENT(INOUT) :: importState, exportState
   TYPE(ESMF_Clock),    INTENT(INOUT) :: clock
   INTEGER,             INTENT(OUT)   :: rc

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

   call wrf_debug ( 100 , 'wrfvar: calling wrfvar ' )
   call da_wrfvar_interface ( head_grid, config_flags )
   call wrf_debug ( 100 , 'wrfvar: back from wrfvar ' )

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_wrfvar_run


