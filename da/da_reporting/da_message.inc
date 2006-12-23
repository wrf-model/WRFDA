subroutine da_message(strs)

   use da_control

   implicit none

   character(len=*), intent(in) :: strs(:)
   integer :: i

#if defined(DM_PARALLEL) && ! defined(STUBMPI)
   do i=1,size(strs) 
      write(unit=stdout,fmt='(a)') trim(strs(i))
   end do
   write(unit=stdout,fmt=*) " "
#endif

end subroutine da_message


