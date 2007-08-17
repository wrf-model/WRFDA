subroutine da_free_unit(unit)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
 
   implicit none

   integer, intent(in) :: unit

   unit_used(unit) = .false.

end subroutine da_free_unit


