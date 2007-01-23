module da_reporting

   use module_wrf_error
   use da_control
   use da_wrf_interfaces

   implicit none

   character(len=300) :: message(100)

contains

#include "da_error.inc"
#include "da_warning.inc"
#include "da_message.inc"
#include "da_message2.inc"

end module da_reporting
