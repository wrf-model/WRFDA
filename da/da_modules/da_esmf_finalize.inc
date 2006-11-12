subroutine da_wrfvar_finalize( gcomp, importState, exportState, clock, rc )

   TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
   TYPE(ESMF_State),    INTENT(INOUT) :: importState, exportState
   TYPE(ESMF_Clock),    INTENT(INOUT) :: clock
   INTEGER,             INTENT(OUT)   :: rc

   !<DESCRIPTION>
   !     WRFVAR finalize routine.
   !
   !     The arguments are:
   !       gcomp           Component
   !       importState     Importstate
   !       exportState     Exportstate
   !       clock           External clock
   !       rc              Return code; equals ESMF_SUCCESS if there are no errors,
   !                       otherwise ESMF_FAILURE.
   !</DESCRIPTION>

   !--output wrfvar analysis

   IF ((config_flags%real_data_init_type == 1) .OR. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_output( head_grid , config_flags )

      call med_shutdown_io ( head_grid , config_flags )
   END IF

   call da_message((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))
   call wrf_shutdown

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_wrfvar_finalize
