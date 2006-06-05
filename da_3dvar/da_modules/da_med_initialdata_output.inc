SUBROUTINE da_med_initialdata_output ( grid , config_flags, out_filename )

   IMPLICIT NONE

   !--Arguments

   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(INOUT) :: config_flags       
   CHARACTER(*),  INTENT (in),  OPTIONAL       :: out_filename

   !--Local

   INTEGER                :: time_step_begin_restart
   INTEGER                :: fid , ierr
   CHARACTER (LEN=80)     :: message
   CHARACTER (LEN=80)     :: file_name

   INTEGER                :: current_day,current_hour,current_month,current_year
   CHARACTER (LEN=1)      :: single_char 
   INTEGER :: julyr, julday
   REAL    :: gmt

   IF (trace_use) CALL da_trace_entry("da_med_initialdata_output")

   IF (print_detail_timing) THEN
      CALL start_timing
   END IF

   if(present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_output'
   endif

   CALL wrf_debug ( 100 , __FILE__//': calling open_w_dataset wrfvar output' )
   CALL open_w_dataset ( fid, TRIM(file_name), grid , config_flags , &
                         output_model_input , "DATASET=INPUT", ierr )

   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , * ) __FILE__//': error opening ', &
         TRIM(file_name),' for writing ierr=',ierr
      CALL wrf_error_fatal( wrf_err_message )
   ENDIF

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   !--------------------------------------------------------------------- 

   CALL output_model_input ( fid, grid , config_flags , ierr )

   CALL close_dataset ( fid , config_flags, "DATASET=INPUT" )

   IF (print_detail_timing) THEN
      WRITE ( message , FMT = '("writing wrfvar output for domain ",I8)' ) grid%id
      CALL end_timing ( TRIM(message) )
   END IF

   IF (trace_use) CALL da_trace_exit("da_med_initialdata_output")

END SUBROUTINE da_med_initialdata_output

