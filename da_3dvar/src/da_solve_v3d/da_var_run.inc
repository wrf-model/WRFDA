   SUBROUTINE da_var_run( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     TYPE(ESMF_State), INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock), INTENT(INOUT) :: clock
     INTEGER, INTENT(OUT) :: rc
!<DESCRIPTION>
!     WRFVAR run routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>


!--do da_var

   CALL wrf_debug ( 100 , 'da_var_run: calling da_var_run ' )
   CALL da_var_solve_interface ( head_grid, config_flags )
   CALL wrf_debug ( 100 , 'da_var_run: back from da_var_run ' )

   ! return success status
   rc = ESMF_SUCCESS

   END SUBROUTINE da_var_run

