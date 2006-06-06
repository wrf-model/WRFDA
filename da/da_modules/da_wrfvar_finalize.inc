SUBROUTINE da_wrfvar_finalize( no_shutdown )
!<DESCRIPTION>
!     WRF finalize routine.
!</DESCRIPTION>
  LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

  !--output wrfvar analysis

  IF((config_flags%real_data_init_type == 1) .or. (config_flags%real_data_init_type == 3)) THEN
    CALL da_med_initialdata_output( head_grid , config_flags )

    CALL med_shutdown_io ( head_grid , config_flags )
    CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )
  END IF

  CALL wrf_debug (   0 , 'wrfvar: SUCCESS COMPLETE WRFVAR' )

  IF (print_detail_timing) THEN
     CALL end_timing ("wrfvar overall") ! start call in wrfvar_init
  END IF

  IF (trace_use) CALL da_trace_report

  IF ( .NOT. PRESENT( no_shutdown ) ) THEN
    ! Finalize time manager
    CALL WRF_UTIL_Finalize
    CALL wrf_shutdown
  END IF

  END SUBROUTINE da_wrfvar_finalize
