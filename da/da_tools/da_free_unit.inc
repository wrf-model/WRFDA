subroutine da_free_unit(unit)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
 
   implicit none

   integer, intent(in) :: unit

   unit_used(unit) = .false.

write (0,*) __FILE__, unit

end subroutine da_free_unit


