subroutine da_get_julian_time(year,month,day,hour,minute,gstime)
!------------------------------------------------------------------------------
!  Purpose: Calculate Julian time from year/month/day/hour/minute.
!
!  HISTORY: 04/2006 - Creation            Jianjun Xu
!
!------------------------------------------------------------------------------

   implicit none

   integer,  intent (in)      :: year
   integer,  intent (in)      :: month
   integer,  intent (in)      :: day
   integer,  intent (in)      :: hour
   integer,  intent (in)      :: minute
   real,     intent (out)     :: gstime

! Local variables
   integer    :: iw3jdn, ndays, nmind
!   nmind = 0
   iw3jdn  =    day - 32075 &
              + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
              + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
              - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
   ndays = iw3jdn - 2443510

   nmind = ndays*1440 + hour * 60 + minute
   gstime = float(nmind)


end subroutine da_get_julian_time
