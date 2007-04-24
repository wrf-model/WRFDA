module da_tools1
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains general tools.
   !---------------------------------------------------------------------------
      
   use da_control, only : unit_used, unit_end, &
      unit_start
   use da_reporting, only : da_error

   implicit none

contains

#include "da_get_unit.inc"
#include "da_free_unit.inc"
   
end module da_tools1

