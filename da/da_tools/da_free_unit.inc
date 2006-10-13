subroutine da_free_unit(unit)
 
   implicit none

   integer, intent(in) :: unit

   unit_used(unit) = .false.

end subroutine da_free_unit
