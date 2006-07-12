subroutine da_error( file_str, line, errors)

   implicit none

   character(len=*), intent(in) :: file_str
   integer ,         intent(in) :: line  ! only print file and line if line > 0
   character(len=*), intent(in) :: errors(:)
   character*256 :: line_str

   write(line_str,'(i6)') line
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
   call wrf_message( '-------------- FATAL callED ---------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      call wrf_message( 'FATAL callED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
   end if
   call da_messages(errors)
   call wrf_message( '-------------------------------------------' )
#else
   call wrf_message2( '-------------- FATAL callED ---------------' )
   ! only print file and line if line is positive
   if ( line > 0 ) then
      call wrf_message2( 'FATAL callED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
   end if
   call da_messages2(errors)
   call wrf_message2( '-------------------------------------------' )
#endif
   call wrf_abort
end subroutine da_error


