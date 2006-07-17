subroutine da_error( file_str, line, errors)

   implicit none

   character(len=*), intent(in) :: file_str
   integer ,         intent(in) :: line  ! only print file and line if line > 0
   character(len=*), intent(in) :: errors(:)
   character*256 :: line_str
   character*256 :: html_file

   write(line_str,'(i6)') line

   html_file=file_str(1:LEN_TRIM(file_str)-4)//'.html'
   
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
   call wrf_message( '-------------- FATAL CALLED ---------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      if (use_html) then
         call wrf_message( 'FATAL CALLED FROM FILE: <A HREF="'// &
            TRIM(documentation_url)//'/'//html_file//'">'//file_str//'</a>  LINE:  '//TRIM(line_str) )
      else
         call wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
      end if
   end if
   call da_messages(errors)
   call wrf_message( '-------------------------------------------' )
#else
   call wrf_message2( '-------------- FATAL callED ---------------' )
   ! only print file and line if line is positive
   if ( line > 0 ) then
      if (use_html) then
         call wrf_message( 'FATAL CALLED FROM FILE: <A HREF="'// &
            TRIM(documentation_url)//'/'//html_file//'">'//file_str//'</a>  LINE:  '//TRIM(line_str) )
      else
         call wrf_message2( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
      end if
   end if
   call da_messages2(errors)
   call wrf_message2( '-------------------------------------------' )
#endif
   call wrf_abort
end subroutine da_error


