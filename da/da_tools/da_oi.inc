! Quick way of showing location
SUBROUTINE da_oi( str,line )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER, INTENT(IN) :: line

  write(0,*) str, line
END SUBROUTINE da_oi
