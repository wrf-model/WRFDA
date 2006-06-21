subroutine da_wrfvar_finalize( no_shutdown )
!<DESCRIPTION>
!     WRF finalize routine.
!</DESCRIPTION>
  LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

  !--output wrfvar analysis

  IF((config_flags%real_data_init_type == 1) .or. (config_flags%real_data_init_type == 3)) THEN
    call da_med_initialdata_output( head_grid , config_flags )

    call med_shutdown_io ( head_grid , config_flags )
    call       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )
  END IF

  call wrf_debug (   0 , 'wrfvar: SUCCESS COMPLETE WRFVAR' )

  IF (print_detail_timing) THEN
     call end_timing ("wrfvar overall") ! start call in wrfvar_init
  END IF

  IF (trace_use) call da_trace_report

  IF ( .NOT. PRESENT( no_shutdown ) ) THEN
    ! Finalize time manager
    call WRF_UTIL_Finalize
    call wrf_shutdown
  END IF

  end subroutine da_wrfvar_finalize
