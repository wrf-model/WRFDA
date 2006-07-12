module da_reporting

   use module_wrf_error

   implicit none

   character(len=200) :: errmsg(100)

contains

#include "da_error.inc"
#include "da_warning.inc"
#include "da_messages2.inc"

end module da_reporting
