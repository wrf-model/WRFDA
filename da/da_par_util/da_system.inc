subroutine da_system(cmd)

   implicit none

   character (len=*) , intent(in) :: cmd
   integer :: ierr

#ifdef crayx1
   call pxfsystem(cmd, len(cmd), ierr)
#else
   call system(cmd)
#end if

end subroutine da_system


