   SUBROUTINE da_var_finalize( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     TYPE(ESMF_State), INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock), INTENT(INOUT) :: clock
     INTEGER, INTENT(OUT) :: rc
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

!--output 3dvar analysis

   if(config_flags%real_data_init_type == 1) then 
     CALL med_initialdata_output_3dvar( head_grid , config_flags )

     CALL med_shutdown_io ( head_grid , config_flags )
     CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )
   endif

   CALL       wrf_debug (   0 , 'da_var: SUCCESS COMPLETE WRFVAR' )
   CALL wrf_shutdown

   ! return success status
   rc = ESMF_SUCCESS

   END SUBROUTINE da_var_finalize

