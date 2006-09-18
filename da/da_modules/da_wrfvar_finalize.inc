subroutine da_wrfvar_finalize( no_shutdown )

   LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

   !--output wrfvar analysis

   IF ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_output( head_grid , config_flags )
      call med_shutdown_io ( head_grid , config_flags )
   END IF

   call da_messages((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))

   IF (trace_use) call da_trace_report

   IF (.NOT. PRESENT(no_shutdown)) THEN
      ! Finalize time manager
      call WRFU_Finalize
      call wrf_shutdown
   END IF

end subroutine da_wrfvar_finalize
