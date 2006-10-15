subroutine da_get_unit(unit)
 
   implicit none

   integer, intent(out) :: unit

   integer i

   unit = -1

   do i = unit_start, unit_end
      if (.NOT. unit_used(i)) then
         unit=i
         unit_used(i) = .true.
         exit
      end if
   end do

   if (unit == -1) then
      call da_error(__FILE__,__LINE__,(/"No free units"/))
   end if

end subroutine da_get_unit
