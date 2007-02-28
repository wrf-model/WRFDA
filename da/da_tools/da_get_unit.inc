subroutine da_get_unit(unit)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
 
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

write (0,*) __FILE__,unit

end subroutine da_get_unit


