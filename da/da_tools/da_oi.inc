subroutine da_oi( str,line )

   ! Quick way of showing location
   IMPLICIT NONE
   CHARACTER*(*) str
   INTEGER, INTENT(IN) :: line

   write(unit=stderr,fmt=*) str, line

end subroutine da_oi


