subroutine da_oi(str,line)

   ! quick way of showing location
   implicit none
   character*(*), intent(in) :: str
   integer, intent(in) :: line

   write(unit=stderr,fmt=*) str, line

end subroutine da_oi


