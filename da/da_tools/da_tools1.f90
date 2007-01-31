module da_tools1
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains general tools.
   !---------------------------------------------------------------------------
      
   use da_control, only : unit_used,use_radiance, stderr, oi_use, unit_end, &
      unit_start
   use da_define_structures, only: ob_type, y_type
   use da_reporting, only : da_error

   implicit none

contains

#include "da_get_unit.inc"
#include "da_free_unit.inc"
#include "da_oi.inc"
   
end module da_tools1

