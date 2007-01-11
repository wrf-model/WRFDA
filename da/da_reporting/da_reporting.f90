module da_reporting

   use module_wrf_error
   use da_control

   implicit none

   character(len=300) :: message(100)

   interface
      subroutine wrf_message(str)
         character(len=*), intent(in) :: str
      end subroutine wrf_message
   end interface

   interface
      subroutine wrf_error_fatal3 (file_str, line, str)
        character(len=*), intent(in) :: file_str
        integer,          intent(in) :: line
        character(len=*), intent(in) :: str(:)
      end subroutine wrf_error_fatal3
   end interface

   interface
      subroutine wrf_check_error(expected, actual, str, file_str, line)
        integer,          intent(in) :: expected
        integer,          intent(in) :: actual
        character(len=*), intent(in) :: str(:)
        character(len=*), intent(in) :: file_str(:)
        integer,          intent(in) :: line
      end subroutine wrf_check_error
   end interface 

   interface
      subroutine wrf_abort
      end subroutine wrf_abort
   end interface 

contains

#include "da_error.inc"
#include "da_warning.inc"
#include "da_message.inc"
#include "da_message2.inc"

end module da_reporting
