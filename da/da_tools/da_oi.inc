! Quick way of showing location
subroutine da_oi( str,line )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER, INTENT(IN) :: line

  write(0,*) str, line
end subroutine da_oi
