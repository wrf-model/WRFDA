subroutine da_med_initialdata_output ( grid , config_flags, out_filename )

   IMPLICIT NONE

   !--Arguments

   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(INOUT) :: config_flags       
   CHARACTER(*),  INTENT (in),  OPTIONAL       :: out_filename

   !--Local

   INTEGER                :: fid , ierr
   CHARACTER (LEN=80)     :: message
   CHARACTER (LEN=80)     :: file_name

   INTEGER :: julyr, julday
   REAL    :: gmt

   IF (trace_use) call da_trace_entry("da_med_initialdata_output")

   if(present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_output'
   endif

   call da_trace ("da_med_initialdata_ouput",Message="Writing wrfvar output")
   call open_w_dataset ( fid, TRIM(file_name), grid , config_flags , &
                         output_model_input , "DATASET=INPUT", ierr )

   IF ( ierr .NE. 0 ) THEN
      WRITE(UNIT=errmsg(1),FMT=*) 'Error opening ', &
         TRIM(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,errmsg(1:1))
   ENDIF

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call output_model_input ( fid, grid , config_flags , ierr )

   call close_dataset ( fid , config_flags, "DATASET=INPUT" )

   IF (trace_use) call da_trace_exit("da_med_initialdata_output")

end subroutine da_med_initialdata_output
