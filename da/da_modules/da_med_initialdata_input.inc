subroutine da_med_initialdata_input (grid, config_flags, filename, in_date)

   IMPLICIT NONE

   ! Arguments

   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type), INTENT(INOUT) :: config_flags

   CHARACTER(*),  INTENT (in)                 :: filename
   CHARACTER(*),  INTENT (in),  OPTIONAL      :: in_date

   ! Local

   INTEGER                 :: fid , ierr , status, n, nsave
   CHARACTER (LEN=80)      :: message

   INTEGER :: julyr, julday
   REAL    :: gmt

   IF (trace_use) call da_trace_entry("da_med_initialdata_input")
   IF (trace_use) call da_trace("da_med_initialdata_input", &
      Message="Reading "//trim(filename))

   ! Initialize the mother domain.

   grid%input_from_file = .true.

   call ext_ncd_open_for_read( trim(filename), 0, 0, "", fid, ierr)

   IF (ierr /= 0) THEN
      write(unit=errmsg(1), fmt='(2a)') &
         'Netcdf error opening file:', trim(filename)
      call da_error(__FILE__,__LINE__,errmsg(1:1))
   endif

   call ext_ncd_get_next_time( fid, current_date, Status )

   if (present(in_date)) then
      ! Set start_date to current_date.
      read(in_date(1:19), fmt='(i4, 5(1x, i2))') &
         grid%start_year,   &
         grid%start_month,  &
         grid%start_day,    &
         grid%start_hour,   &
         grid%start_minute, &
         grid%start_second 

      nsave = -1
      DO n=1, 1000
         if (current_date(1:19) == in_date(1:19)) then
            nsave = n - 1
            exit
         endif
         call ext_ncd_get_next_time( fid, current_date, Status )
      enddo

      if (nsave < 0) then
         call wrf_error_fatal("Can not find the needed time")
      endif
   else
      ! Set start_date to current_date.
      read(current_date(1:19), fmt='(i4, 5(1x, i2))') &
           grid%start_year,  &
           grid%start_month, &
           grid%start_day,   &
           grid%start_hour,  &
           grid%start_minute,&
           grid%start_second
   endif

   call geth_julgmt(julyr, julday, gmt)
   call nl_set_gmt (grid%id, gmt)
   call nl_set_julyr (grid%id, julyr)
   call nl_set_julday (grid%id, julday)

   call nl_set_iswater (grid%id, grid%iswater )
   call nl_set_cen_lat ( grid%id , grid%cen_lat )
   call nl_set_cen_lon ( grid%id , grid%cen_lon )
   call nl_set_truelat1 ( grid%id , grid%truelat1 )
   call nl_set_truelat2 ( grid%id , grid%truelat2 )
   call nl_set_moad_cen_lat ( grid%id , grid%moad_cen_lat )
   call nl_set_stand_lon ( grid%id , grid%stand_lon )
   call nl_set_map_proj ( grid%id , grid%map_proj )

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call ext_ncd_ioclose(fid, ierr)

   call da_trace("da_med_initialdata_input",message="open_r_dataset for "//TRIM(filename))
   call open_r_dataset ( fid, TRIM(filename), grid , config_flags , &
      "DATASET=INPUT", ierr )

   IF ( ierr .NE. 0 ) THEN
     WRITE(UNIT=errmsg(1),FMT='(A,A,A,I5)') 'Error opening ', &
       TRIM(filename),' for reading ierr=',ierr
     call da_error(__FILE__,__LINE__,errmsg(1:1))
   ENDIF

   if (present(in_date)) then
      do n=1, nsave
         call da_messages((/"current_date="//trim(current_date)// &
            ', in_date='//trim(in_date)/))
         call ext_ncd_get_next_time( fid, current_date, Status )
      enddo
   endif

   call input_model_input ( fid ,   grid , config_flags , ierr )

   call close_dataset ( fid , config_flags , "DATASET=INPUT" )

   IF (trace_use) call da_trace_exit("da_med_initialdata_input")

end subroutine da_med_initialdata_input


