subroutine trace_stop(myid, my_html, my_csv, my_url)

   implicit none

   integer, intent(in) :: myid
   logical, optional, intent(in) :: my_html, my_csv
   character(LEN=*), optional, intent(in) :: my_url

   real :: traceBeginTime, diff

   call trace_time(traceBeginTime)
   
   call date_and_time(DATE=traceStopDate,ZONE=traceTimeZone)
   call date_and_time(TIME=traceStopTime)

   call trace_time(traceCurrentTime)

   call trace_report

   write(unit=0, fmt='(4a)') &
        'Trace started at: ', traceStartDate, ':', traceStartTime, &
        'Trace stoped  at: ', traceStopDate,  ':', traceStopTime

   call trace_time(traceCurrentTime)
   write(unit=0, fmt='(a, f12.3)') &
        'Trace last time:', traceCurrentTime - traceBaseTime

   call trace_time(traceCurrentTime)
   call trace_diff_time(traceBeginTime, traceCurrentTime, diff)
   traceUsedTime = traceUsedTime + diff

   write(unit=0, fmt='(a, f12.3)') &
        'Trace used time:', traceUsedTime

end subroutine trace_stop

