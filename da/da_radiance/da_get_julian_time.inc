subroutine da_get_julian_time(year,month,day,hour,minute,gstime)
!------------------------------------------------------------------------------
!  PURPOSE: Calculate Julian time from year/month/day/hour/minute.
!
!  HISTORY: 04/2006 - Creation            Jianjun Xu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,  INTENT (in)      :: year
   INTEGER,  INTENT (in)      :: month
   INTEGER,  INTENT (in)      :: day
   INTEGER,  INTENT (in)      :: hour
   INTEGER,  INTENT (in)      :: minute
   REAL,     INTENT (out)     :: gstime

! Local variables
   INTEGER    :: iw3jdn, ndays, nmind
!   nmind = 0
   iw3jdn  =    day - 32075 &
              + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
              + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
              - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
   ndays = iw3jdn - 2443510

   nmind = ndays*1440 + hour * 60 + minute
   gstime = float(nmind)


end subroutine da_get_julian_time


