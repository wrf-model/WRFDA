subroutine da_wrfvar_finalize

   ! output wrfvar analysis

   IF ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_output( head_grid , config_flags )
      call med_shutdown_io ( head_grid , config_flags )
   END IF

   call da_messages((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))

end subroutine da_wrfvar_finalize
