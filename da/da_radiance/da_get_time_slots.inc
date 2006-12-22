subroutine da_get_time_slots(nt,tmin,tana,tmax,time_slots)
!------------------------------------------------------------------------------
!  PURPOSE: Calculate time slots for FGAT option.
!
!  HISTORY: 04/14/2006 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,           INTENT (in)      :: nt    ! number of time slots
   CHARACTER(LEN=19), INTENT (in)      :: tmin  ! begin of time window
   CHARACTER(LEN=19), INTENT (in)      :: tana  ! center of time window
   CHARACTER(LEN=19), INTENT (in)      :: tmax  ! end of time window
   REAL,             INTENT (out)      :: time_slots(0:nt) ! 

! Local variables
   INTEGER   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   INTEGER   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   CHARACTER :: s
   REAL      :: dt
   INTEGER   :: it

   if (trace_use) call da_trace_entry("da_get_time_slots")

   read(UNIT=tmin,FMT='(i4,5(a1,i2))') min_yyyy,s,min_mm,s,min_dd,s,min_hh,s,min_mn,s,min_ss
   read(UNIT=tmax,FMT='(i4,5(a1,i2))') max_yyyy,s,max_mm,s,max_dd,s,max_hh,s,max_mn,s,max_ss

   write(UNIT=stdout,FMT=*) ' Start Julian Time : ',min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   write(UNIT=stdout,FMT=*) ' End   Julian Time : ',max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss

   call da_get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,time_slots(0))
   call da_get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,time_slots(nt))

   if ( nt > 1) then
     dt = (time_slots(nt)-time_slots(0))/float(nt-1)
     time_slots(1)  = time_slots(0)+dt*0.5
     do it=2,nt-1
       time_slots(it) = time_slots(it-1)+dt
     enddo
   end if

   if (print_detail_radiance) then
      write(UNIT=stdout,FMT='(a,12f10.0)') ' time_slots ', time_slots(0:nt)
   end if

   if (trace_use) call da_trace_exit("da_get_time_slots")

end subroutine da_get_time_slots


