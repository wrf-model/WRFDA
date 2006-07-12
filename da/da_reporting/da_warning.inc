subroutine da_warning( file_str, line, warnings )

   implicit none

   character(len=*), intent(in) :: file_str
   integer,          intent(in) :: line
   character(len=*), intent(in) :: warnings(:)
   character*256 :: line_str

   write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
   call wrf_message( '-------------- WARNING      ---------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      call wrf_message( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
   end if
   call wrf_messages(warnings)
   call wrf_message( '-------------------------------------------' )
#else
   call wrf_message2( '-------------- WARNING ---------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      call wrf_message2( 'WARNING FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
   end if
   call da_messages2(warnings)
   call wrf_message2( '-------------------------------------------' )
#endif

end subroutine da_warning


