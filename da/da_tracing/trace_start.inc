subroutine trace_start(myid, my_html, my_csv, my_url)

   implicit none

   integer, intent(in) :: myid
   logical, optional, intent(in) :: my_html, my_csv
   character(len=*), optional, intent(in) :: my_url

   integer :: m, n

   call trace_time(traceBaseTime)

   call date_and_time(DATE=traceStartDate,ZONE=traceTimeZone)
   call date_and_time(TIME=traceStartTime)

   write(unit=0, fmt='(4a)') &
        'Trace started at: ', traceStartDate, ':', traceStartTime

   myproc = myid

   if(present(my_html)) then
      use_html = my_html
   endif

   if(present(my_csv)) then
      use_csv = my_csv
   endif

   if(present(my_url)) then
      documentation_url = my_url
   endif

   traceDepth   = 0
   tracerID     = 0
   totalRoutine = 0

   do n=0, maxRoutines
      tracerBeingCalled(n) = 0
      tracerParentID(n)    = 0
   enddo

   do n=0, maxRoutines
      tracerCPUTime(n) = 0.0
      tracerElapsedTime(n) = 0.0
   enddo

   do n=0, maxRoutines
      tracerChildCounter(n) = 0
      tracerParentCounter(n) = 0
   enddo

   do n=0, maxRoutines
   do m=1, traceMaxDepth
      tracerChildList(m,n)  = 0
      tracerParentList(m,n) = 0
   enddo
   enddo

   call trace_time(traceCurrentTime)
   call trace_diff_time(traceBaseTime, traceCurrentTime, traceUsedTime)

end subroutine trace_start

