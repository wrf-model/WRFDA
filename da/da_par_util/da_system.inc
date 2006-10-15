SUBROUTINE da_system(cmd)

   IMPLICIT NONE
   CHARACTER (LEN=*) , INTENT(IN) :: cmd
   integer :: ierr
#ifdef crayx1
   call pxfsystem(cmd, len(cmd), ierr)
#else
   call system(cmd)
#endif

END SUBROUTINE da_system
