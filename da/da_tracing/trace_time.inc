subroutine trace_time(seconds)

   implicit none

   real, intent(out) :: seconds

!--CALL DATE_AND_TIME([DATE, TIME, ZONE, VALUES])
!--DATE 	(Optional) The type shall be CHARACTER(8) or larger.
!--TIME 	(Optional) The type shall be CHARACTER(10) or larger.
!--ZONE 	(Optional) The type shall be CHARACTER(5) or larger.
!--VALUES	(Optional) The type shall be INTEGER(8).
!--VALUE(1): 	The year
!--VALUE(2): 	The month
!--VALUE(3): 	The day of the month
!--VAlUE(4): 	Time difference with UTC in minutes
!--VALUE(5): 	The hour of the day
!--VALUE(6): 	The minutes of the hour
!--VALUE(7): 	The seconds of the minute
!--VALUE(8): 	The milliseconds of the second

!--character(8)  :: date
!--character(10) :: time
!--character(5)  :: zone

   integer,dimension(8) :: values
!--using keyword arguments
!--call date_and_time(date,time,zone,values)
!--call date_and_time(DATE=date,ZONE=zone)
!--call date_and_time(TIME=time)
   call date_and_time(VALUES=values)
!--print '(a,2x,a,2x,a)', date, time, zone
!  write(unit=0, fmt='(a,i4,5(a,i2.2),a,i3.3)') &
!       'Time: ', values(1), '_', values(2), '_', values(3), '_', values(5), &
!       ':', values(6), ':', values(7), ':', values(8)

   seconds = real(60*(60*values(5) + values(6)) + values(7)) + 0.001*values(8)

!  write(unit=0, fmt='(a,f10.2)') &
!       'Time in seconds:', seconds

end subroutine trace_time

