module da_reporting

   use da_control, only : stdout, use_html, documentation_url, &
      warnings_are_fatal

   use da_wrf_interfaces, only : wrf_message, wrf_abort

   implicit none

   character(len=10000) :: message(50)

contains

#include "da_error.inc"
#include "da_warning.inc"
#include "da_message.inc"
#include "da_message2.inc"

end module da_reporting
