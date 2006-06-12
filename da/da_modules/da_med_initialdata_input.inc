SUBROUTINE da_med_initialdata_input (grid, config_flags, in_date, in_filename)

   IMPLICIT NONE

   !--Arguments

   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags

   CHARACTER(*),  INTENT (in),  OPTIONAL      :: in_date
   CHARACTER(*),  INTENT (in),  OPTIONAL      :: in_filename

   !--Local

   INTEGER                 :: fid , ierr , status, n, nsave, previous_status
   CHARACTER (LEN=80)      :: file_name, previous_time_string
   CHARACTER (LEN=80)      :: message
   CHARACTER (LEN= 1)      :: char

   INTEGER :: julyr, julday
   REAL    :: gmt

   !------------------------------------------------------------------------------

   IF (trace_use) CALL da_trace_entry("da_med_initialdata_input")

   ! Initialize the mother domain.

   IF (print_detail_timing) THEN
      CALL start_timing
   END IF

   grid%input_from_file = .true.

   IF (present(in_filename)) THEN
      file_name = trim(in_filename)
   ELSE
      file_name = 'wrfvar_input'
   END IF

   CALL ext_ncd_open_for_read( trim(file_name), 0, 0, "", fid, ierr)

   IF (ierr /= 0) THEN
      write(unit=errmsg(1), fmt='(2a)') &
         'Netcdf error opening file:', trim(file_name)
      CALL wrf_error(__FILE__,__LINE__,errmsg(1))
   endif

   CALL ext_ncd_get_next_time( fid, current_date, Status )

   if (present(in_date)) then
      !-----Set start_date to current_date.
      read(in_date(1:19), fmt='(i4, 5(a1, i2))') &
         grid%start_year,  char, &
         grid%start_month, char, &
         grid%start_day,   char, &
         grid%start_hour,  char, &
         grid%start_minute,char, &
         grid%start_second 

      nsave = -1
      DO n=1, 1000
        message = 'current_date='//trim(current_date)// &
           ', in_date='//trim(in_date)
         CALL wrf_debug ( 100 , trim(message) )
         if(current_date(1:19) == in_date(1:19)) then
            nsave = n - 1
            exit
         endif
         CALL ext_ncd_get_next_time( fid, current_date, Status )
      enddo

      if(nsave < 0) then
         CALL wrf_error_fatal("Can not find the needed time")
      endif
   else
      !-----Set start_date to current_date.
      read(current_date(1:19), fmt='(i4, 5(a1, i2))') &
           grid%start_year,  char, &
           grid%start_month, char, &
           grid%start_day,   char, &
           grid%start_hour,  char, &
           grid%start_minute,char, &
           grid%start_second
   endif

   call geth_julgmt(julyr, julday, gmt)
   CALL nl_set_gmt (grid%id, gmt)
   CALL nl_set_julyr (grid%id, julyr)
   CALL nl_set_julday (grid%id, julday)

   CALL nl_set_iswater (grid%id, grid%iswater )
   CALL nl_set_cen_lat ( grid%id , grid%cen_lat )
   CALL nl_set_cen_lon ( grid%id , grid%cen_lon )
   CALL nl_set_truelat1 ( grid%id , grid%truelat1 )
   CALL nl_set_truelat2 ( grid%id , grid%truelat2 )
   CALL nl_set_moad_cen_lat ( grid%id , grid%moad_cen_lat )
   CALL nl_set_stand_lon ( grid%id , grid%stand_lon )
   CALL nl_set_map_proj ( grid%id , grid%map_proj )

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call ext_ncd_ioclose(fid, ierr)

   CALL wrf_debug ( 100 , __FILE__//': calling open_r_dataset for wrfvar input' )
   CALL open_r_dataset ( fid, TRIM(file_name), grid , config_flags , &
      "DATASET=INPUT", ierr )

   IF ( ierr .NE. 0 ) THEN
     WRITE( wrf_err_message , * ) __FILE__//': error opening ', &
       TRIM(file_name),' for reading ierr=',ierr
     CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF

   if (present(in_date)) then
      do n=1, nsave
         message = 'current_date='//trim(current_date)// &
            ', in_date='//trim(in_date)
         CALL wrf_debug ( 100 , trim(message) )
         CALL ext_ncd_get_next_time( fid, current_date, Status )
      enddo
   endif

   CALL wrf_debug ( 100 , __FILE__//': calling input_model_input' )
   CALL input_model_input ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug ( 100 , __FILE__//': back from input_model_input' )

   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   if (print_detail_timing) THEN
      WRITE ( message , FMT = '("reading wrfvar input for domain ",I8)' ) &
         grid%id
      CALL end_timing ( TRIM(message) )
   END IF

   IF (trace_use) CALL da_trace_exit("da_med_initialdata_input")

END SUBROUTINE da_med_initialdata_input
