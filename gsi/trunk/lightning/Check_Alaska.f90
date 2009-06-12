
Subroutine  Check_Alaska(numStrike,xlon,ylat,time,strike,lquality,analysis_time)
!
!  Author: Ming Hu, ESRL/GSD
!  
!  First written: 01/02/2008.
!
!  IN:
!     numStrike
!     xlon
!     ylat
!     time
!     strike
!     
!  out:
!     lquality
!
  IMPLICIT NONE

  INTEGER ::   numStrike

  REAL ::   xlon(numStrike)
  REAL ::   ylat(numStrike)
  character*21 ::   time(numStrike)
  integer   ::   strike(numStrike)

  integer   ::   lquality(numStrike)

  character*10 ::   analysis_time

  integer   :: i,j

  integer :: anl_hh
  integer :: obs_hh, obs_ss

!
!   check duplicate
!
  DO i=2,numStrike
    DO j=1,i-1
      if(abs(xlon(i) - xlon(j)) < 0.001 .and.   &
         abs(ylat(i) - ylat(j)) < 0.001 ) then
         if(time(i) == time(j) ) then
           lquality(i)=1
         endif
      endif
    ENDDO
  enddo

!
!  check if strike number > 0
!
  DO i=1,numStrike
      if(strike(i) <= 0 ) then
        lquality(i)=1
      endif
  enddo
!
!   check time
!
  read(analysis_time(9:10),'(i2)') anl_hh
  DO i=1,numStrike
     read(time(i)(10:11),'(I2)') obs_hh
     read(time(i)(13:14),'(I2)') obs_ss
     if( anl_hh == obs_hh ) then
       if(obs_ss > 10 ) then
         lquality(i)=1
       endif
     else
       if(obs_ss < 30 ) then
         lquality(i)=1
       endif
     endif
  enddo

!
!  longitude should use negative value
!
  DO i=1,numStrike
    if(xlon(i) < 180) then
         xlon(i)= -xlon(i)
     else
         write(*,*) ' positive longitude in Alaksa , what to do????'
         stop 1234
     endif
  enddo
!
end subroutine Check_Alaska

