module module_trace

   interface
      ! c code
      subroutine da_memory(memory_used)
         integer, intent(out) :: memory_used
      end subroutine da_memory
   end interface

   implicit none

   integer, parameter :: traceIndentAmount = 2    ! default indent
   integer, parameter :: traceNameLen      = 256  ! Length of trace name
   integer, parameter :: traceMaxDepth     = 100  ! Length of trace name
   integer, parameter :: maxRoutines       = 4096 ! maxium number of subroutines

   character(LEN=TraceNameLen) :: documentation_url = "DOCUMENTATION_URL"

   character(LEN= 5) :: traceTimeZone
   character(LEN= 8) :: traceStartDate, traceStopDate
   character(LEN=10) :: traceStartTime, traceStopTime

   logical :: use_html = .true., &
              use_csv = .true.

   integer :: myproc = 0

!--Variables
   integer :: trace_unit     = 3
   integer :: trace_csv_unit = 4

   integer :: totalRoutine = 0

   integer :: ierr = 0

   integer :: traceDepth     = 0      ! Current depth of trace
   integer :: tracerID       = 0      ! Number of routines so far

   character(len=traceNameLen), dimension(0:maxRoutines) :: subroutineNames

   integer, dimension(0:maxRoutines) :: tracerBeingCalled

   integer, dimension(0:maxRoutines) :: tracerParentID

   integer, dimension(traceMaxDepth, 0:maxRoutines) :: tracerChildList, tracerParentList
   integer, dimension(0:maxRoutines) :: tracerChildCounter, tracerParentCounter

!--All counts
   real :: traceBaseTime, traceCurrentTime, traceUsedTime

   real, dimension(0:MaxRoutines) :: tracerStartTime, &
                                     tracerCPUTime, &
                                     tracerStartElapsedTime, &
                                     tracerElapsedTime

   logical :: NewTraceRoutine = .true.

contains

subroutine trace_diff_time(sec1, sec2, diff)

   implicit none

   real, intent(in)  :: sec1, sec2
   real, intent(out) :: diff
         
   diff = sec2 - sec1

   do while (diff < 0.0)
      diff = diff + 86400.0
   enddo

end subroutine trace_diff_time

subroutine trace_entry( Name, &       ! in
                        Message )     ! in, optional

   !-----------------------------------------------------------------------
   ! Purpose: Trace entry point to subroutine
   !-----------------------------------------------------------------------

   implicit none

   character (len=*),           intent(in) :: Name         ! Routine name
   character (len=*), optional, intent(in) :: Message      ! message


   real    :: current_time, diff
   integer :: n, previd
   logical :: NewTraceRoutine = .true.

   call trace_time(current_time)

   traceDepth = traceDepth + 1

   previd = tracerID

!--Check if this routine being called before
   NewTraceRoutine = .true.
   do n=1, totalRoutine
      if(trim(subroutineNames(n)) == trim(name)) then
         NewTraceRoutine = .false.
         tracerID = n
         exit
      endif
   enddo

!--It is a new routine.
   if(NewTraceRoutine) then
      totalRoutine = totalRoutine + 1
      tracerID = totalRoutine
      subroutineNames(tracerID) = trim(name)
   endif

!  write(unit=0, fmt='(3a,i4,a,f12.2)') &
!       'enter ', trim(name), ' as id:', tracerID, ', at: ', current_time

!--stop previd timer
   call trace_diff_time(tracerStartTime(previd), current_time, diff)
   tracerCPUTime(previd) = tracerCPUTime(previd) + diff

!--Remember its parent ID.
   tracerParentID(tracerID) = previd

!--Increase the number of current routine being called.
   tracerBeingCalled(tracerID) = tracerBeingCalled(tracerID) + 1

!--Just start record the time
   tracerStartTime(tracerID) = current_time
   tracerStartElapsedTime(tracerID) = current_time

!--record the caller
   NewTraceRoutine = .true.
   do n = 1, tracerParentCounter(tracerID)
      if(tracerParentList(n, tracerID) == tracerID) then
         NewTraceRoutine = .false.
         exit
      endif
   enddo

   if(NewTraceRoutine) then
      n = tracerParentCounter(tracerID) + 1
      tracerParentList(n, tracerID) = previd
      tracerParentCounter(tracerID) = n
   endif

!--record the callee
   NewTraceRoutine = .true.
   do n = 1, tracerChildCounter( previd)
      if(tracerChildList(n, tracerID) == previd) then
         NewTraceRoutine = .false.
         exit
      endif
   enddo

   if(NewTraceRoutine) then
      n = tracerChildCounter(tracerID) + 1
      tracerChildList(n, previd) = tracerID
      tracerChildCounter(tracerID) = n
   endif

   call trace_time(traceCurrentTime)
   call trace_diff_time(current_time, traceCurrentTime, diff)
   traceUsedTime = traceUsedTime + diff

end subroutine trace_entry

subroutine trace_error( file_str, line, errors )

   !-----------------------------------------------------------------------
   ! Purpose: Standardised error reporting
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: file_str
   integer ,         intent(in) :: line  ! only print file and line if line > 0
   character(len=*), intent(in) :: errors(:)

   character(len=256) :: line_str, html_file

   integer       :: i

   write(line_str,'(i6)') line

   html_file=file_str(1:LEN_trim(file_str)-4)//'.html'
   
   call wrf_message( &
      '---------------------------- FATAL ERROR -----------------------' )

!--only print file and line if line is positive
   if ( line > 0 ) then
      if (use_html) then
         call wrf_message( 'Fatal error in file: <A HREF="'// &
            trim(documentation_url)//'/'//trim(html_file)//'">'// &
            trim(file_str)//'</a>  LINE:  '//trim(line_str) )
      else
         call wrf_message( 'Fatal error in file:  '//trim(file_str)// &
            '  LINE:  '//trim(line_str) )
      end if
   end if

   do i=1,size(errors)
      call wrf_message(errors(i))
   end do

   call wrf_message( &
      '----------------------------------------------------------------' )

   call wrf_abort

end subroutine trace_error

subroutine trace_exit( Name, &             ! in
                       Message )           ! in, optional

!-----------------------------------------------------------------------
!--Purpose: Trace exit from subroutine
!-----------------------------------------------------------------------

   implicit none

   character (len=*), intent(in)           :: Name         ! subroutine name
   character (len=*), optional, intent(in) :: Message      ! text to trace

   real :: current_time, diff

   call trace_time(current_time)

!  write(unit=0, fmt='(3a,i4,a,f12.2)') &
!       'exit  ', trim(name), ' as id:', tracerID, ', at: ', current_time

   traceDepth=traceDepth-1

!=======================================================================
!--Check timing and maximum heap memory usage
!=======================================================================

   call trace_diff_time(tracerStartTime(tracerID), current_time, diff)
   tracerCPUTime(tracerID) = tracerCPUTime(tracerID)  + diff

   call trace_diff_time(tracerStartElapsedTime(tracerID), current_time, diff)
   tracerElapsedTime(tracerID) = tracerElapsedTime(tracerID)  + diff

!--Reset tracerID to its parent.
   tracerID = tracerParentID(tracerID)

   call trace_time(traceCurrentTime)
   tracerStartTime(tracerID) = traceCurrentTime

   call trace_diff_time(current_time, traceCurrentTime, diff)
   traceUsedTime = traceUsedTime + diff

end subroutine trace_exit

subroutine trace_info( name,     &           ! in
                       message)              ! in, optional

   implicit none

!--------------------------------------------------------------------
!--Purpose: General trace within a subroutine
!--------------------------------------------------------------------

   character (len=*), intent(in)           :: name         ! Subroutine name
   character (len=*), optional, intent(in) :: message      ! Text to trace

   integer           :: IOStatus     ! I-O return code
   integer           :: Loop         ! General loop counter
   integer           :: TotalSpace
   character(len=25) :: Change

!-----------------------------------------------------------------------
!--Check depth of trace
!-----------------------------------------------------------------------

   if (TraceDepth > TraceMaxDepth) then
      write(unit=0, fmt='(2a)') 'Reached TraceMaxDepth for routine: ', trim(name)
      return
   end if

end subroutine trace_info

subroutine trace_int_sort( key, n, index)

!----------------------------------------------------------------------
!--Purpose: sort integers for tracing
!----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n      ! The number of items to be sorted. 
   integer, intent(in)  :: key(:)
   integer, intent(out) :: index(:)

   integer :: head       ! heaps are tree structures: head and child refer
   integer :: child      ! to related items within the tree 
   integer :: i          
   integer :: dum        ! used to swap index items


!--initialise index:
   do i=1,n
      index(i)=i
   end do 

!--Do heapsort: Create the heap...
   makeheap : do i=n/2,1,-1
      head=i
      sift1 : do
         !-find the largest out of the head and its two children...
         child=head*2
         if (child>n) exit sift1
         if (child<n) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if

         !-if the head is the largest, then sift is done...
         if (key(index(head))>=key(index(child))) exit sift1

         !-otherwise swap to put the largest child at the head,
         !-and prepare to repeat the procedure for the head in its new
         !-subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift1
   end do makeheap

!--Retire heads of the heap, which are the largest, and
!--stack them at the end of the array.
   retire : do i=n,2,-1
      dum=index(1)
      index(1)=index(i)
      index(i)=dum
      head=1
         ! second sift is similar to first...
      sift2: do
         child=head*2
         if (child>(i-1)) exit sift2
         if (child<(i-1)) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if
         if (key(index(head))>=key(index(child))) exit sift2
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift2  
   end do retire

end subroutine trace_int_sort

subroutine trace_real_sort(key, n, index)

!-----------------------------------------------------------------------
!--Purpose: Sort reals for tracing
!-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n      ! The number of items to be sorted. 
   real,    intent(in)  :: key(1:n)
   integer, intent(out) :: index(1:n)

   integer :: head       ! heaps are tree structures: head and child refer
   integer :: child      ! to related items within the tree 
   integer :: i          
   integer :: dum        ! used to swap index items

!--initialise index:
   do i=1,n
      index(i)=i
   end do

!--Do heapsort: Create the heap...
   makeheap : do i=n/2,1,-1
      head=i
      sift1 : do
         !-find the largest out of the head and its two children...
         child=head*2
         if (child>n) exit sift1
         if (child<n) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if

         !-if the head is the largest, then sift is done...
         if (key(index(head))>=key(index(child))) exit sift1

         !-otherwise swap to put the largest child at the head,
         !-and prepare to repeat the procedure for the head in its new
         !-subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift1
   end do makeheap

!--Retire heads of the heap, which are the largest, and
!--stack them at the end of the array.

   retire : do i=n,2,-1
      dum=index(1)
      index(1)=index(i)
      index(i)=dum
      head=1
      !-second sift is similar to first...
      sift2: do
         child=head*2
         if (child>(i-1)) exit sift2
         if (child<(i-1)) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if

         if (key(index(head))>=key(index(child))) exit sift2
         dum=index(child)  
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift2  
   end do retire

end subroutine trace_real_sort

subroutine trace_report

!--------------------------------------------------------------------
!-Purpose: Produce a trace report
!--------------------------------------------------------------------

   implicit none

   integer :: i, j, current_tracer
   integer, allocatable :: index(:)

   real :: current_time
   real :: totalElapsedTime
   real :: totalCPUTime
   real :: percentCPUTime            !--percentage in CPU time
   real :: percentElapsedTime        !--percentage in elapsed time
   real :: avgfactor

   integer :: IOStatus             ! I/O return code
   character (len=200) :: TraceFile
   character (len=200) :: csvname
   character (len=10)  :: temp

   call trace_time(current_time)

   call trace_diff_time(traceBaseTime, current_time, totalElapsedTime)

   totalCPUTime = totalElapsedTime - traceUsedTime

   if(totalElapsedTime < 0.001) then
      totalElapsedTime = 0.001
   endif

   if(totalCPUTime < 0.001) then
      totalCPUTime = 0.001
   endif

!==========================================================================
!--Sort subroutines into time order based on local Elapsed Time.
!--All PEs should have the same sort order after the sum.
!==========================================================================

   allocate (index(1:totalRoutine))

!-----------------------------------------------------------------
!--Open trace output file. 
!-----------------------------------------------------------------

   if(trace_unit == 0) then
      trace_unit = 7
   endif

   write(unit=temp,fmt='(i10)') myproc

   if (use_html) then
      traceFile="trace/"//trim(adjustl(temp))//".html"
   else   
      tracefile="trace/"//trim(adjustl(temp))//".txt"
   end if

   open(unit=trace_unit, &      ! i:
        file=trim(tracefile), & ! i:
        status="unknown", &     ! i:
        action="write", &       ! i:
        iostat=IOStatus)        ! O:

   if (IOStatus /= 0) then
      call trace_error("trace_report.inc",68, &
         (/"Cannot open trace file "//traceFile/))
   endif

   if(use_csv) then
      write(unit=csvname,fmt='(I10,A)') myproc,'.csv'
      open(unit=trace_csv_unit,file="trace/"//trim(adjustl(csvname)), &
         status="unknown",iostat=IOStatus)
      if (IOStatus /= 0) then
         call trace_error("trace_report.inc",77,(/"Cannot open "//csvname/))
      endif
   endif

   if(use_html) then
      write(unit=trace_unit, fmt='(A)') "<html><head><title>Tracing</title></head>"
      write(unit=trace_unit, fmt='(A)') "<body><h1>Trace Output</h1>"
      write(unit=trace_unit, fmt='(A)') "<ul>"
      write(unit=trace_unit, fmt='(A)') "<li><a href=#tree>Calling Tree</a>"
      write(unit=trace_unit, fmt='(A)') "<li><a href=#local>Local routine timings</a>"
      write(unit=trace_unit, fmt='(A)') "<li><a href=#overall>Overall routine timings</a>"
      write(unit=trace_unit, fmt='(A)') "<li><a href=#memory>Memory usage</a>"
      write(unit=trace_unit, fmt='(A)') "</ul>"
      write(unit=trace_unit, fmt='(A)') "<a name=tree><h2>Calling Tree</h2></a><pre>"
   else
      write(unit=trace_unit, fmt='(A)') "Trace Output"
      write(unit=trace_unit, fmt='(A)') ""
   endif

   write(unit=trace_unit, fmt='(A,I4,A)') &
      "</pre><hr><H1>For PE",myproc,"</H1>"

!--Output timing information

   do j=1, 2
      if(j == 1) then
         call trace_real_sort(tracerCPUTime(1:totalRoutine),totalRoutine,index)
         write(unit=trace_unit, fmt='(a)') &
              "<a name=elapsedtime><h2>Timing Summary, Sorted in CPU Time</h2></a>"
      else if(j == 2) then
         call trace_real_sort(tracerElapsedTime(1:totalRoutine), totalRoutine, index)
         write(unit=trace_unit, fmt='(a)') &
              "<a name=elapsedtime><h2>Timing Summary, Sorted in Elapsed Time</h2></a>"
      else
         exit
      endif

      write(unit=trace_unit, fmt='(a, f12.3, a)') &
           "(Tracing itself took ", traceUsedTime, &
           "s CPU Time. This is not included in the times below.)<p>"

      write(unit=trace_unit, fmt='(a)') &
           "<TABLE BORDER>", &
           "<TR>", &
           "   <TH>Routine Name</TH>", &
           "   <TH>Calls</TH>", &
           "   <TH>Total Elapsed Time (seconds)</TH>", &
           "   <TH>percentElapsed</TH>", &
           "   <TH>Average Elapsed Time (seconds)</TH>", &
           "   <TH>Total CPU Time (seconds)</TH>", &
           "   <TH>percentCPU</TH>", &
           "   <TH>Average CPU Time (seconds)</TH>", &
           "</TR>"

      do i=totalRoutine, 1, -1
         current_tracer=index(i)    

         percentCPUTime=100.0 * tracerCPUTime(current_tracer)/totalCPUTime
         percentElapsedTime=100.0*tracerElapsedTime(current_tracer)/totalElapsedTime

!------------------------------------------------------------------------
!--------Write out results. Note the abnormally long format line is needed as
!--------the NAG compiler complains if a quoted line is split.
!------------------------------------------------------------------------

         write(unit=trace_unit, fmt='(a)') &
              "<TR>"

         write(unit=trace_unit, fmt='(7a)') &
              "<TD><a href=", &
              trim(Documentation_url), &
              "/", &
              trim(subroutineNames(current_tracer)), & ! Subroutine name
              ".html>", &
              trim(subroutineNames(current_tracer)), & ! Subroutine name
              "</a>"

         avgfactor = 1.0/real(tracerBeingCalled(current_tracer))

         write(unit=trace_unit, fmt='(3x,a,i10,a)') &
               "<TD>", tracerBeingCalled(current_tracer), "</TD>"
         write(unit=trace_unit, fmt='(3x,a,f12.3,a)') &
               "<TD>", tracerElapsedTime(current_tracer), "</TD>", &
               "<TD>", percentElapsedTime, "</TD>", &
               "<TD>", tracerElapsedTime(current_tracer)*avgfactor, "</TD>", &
               "<TD>", tracerCPUTime(current_tracer), "</TD>", &
               "<TD>", percentCPUTime, "</TD>", &
               "<TD>", tracerCPUTime(current_tracer)*avgfactor, "</TD>"

         if(use_csv) then
            write(unit=trace_csv_unit, fmt='(a,2x,a40,a,2x,i8,10(a,f12.3))') &
                 'local,', &
                 trim(subroutineNames(current_tracer)), &
                 ',', tracerBeingCalled(current_tracer), &
                 ",", tracerElapsedTime(current_tracer), &
                 ",", percentElapsedTime, &
                 ",", tracerElapsedTime(current_tracer)*avgfactor, &
                 ",", tracerCPUTime(current_tracer), &
                 ",", tracerCPUTime(current_tracer)*avgfactor, &
                 ",", percentCPUTime
         endif
      enddo

      write(unit=trace_unit, fmt='(a,i6,a,f12.3,a,f12.3,a)') &
         "<TR><TD><B>Total</B></TD><TD>",totalRoutine, "</TD><TD><B>", &
         totalElapsedTime, &
         "</B></TD><TD></TD><TD></TD><TD><B>", &
         TotalCPUTime, &
         "</B></TD><TD></TD><TD></TD>"

      write(unit=trace_unit, fmt='(a)') &
           "</TABLE><p><p>"

      if(use_csv) then
         write(unit=trace_csv_unit, fmt='(a)') " "
      end if
   enddo

   if(trace_unit /= 0) then
      close(trace_unit)
   endif
  
   if(use_csv) then
      close(trace_csv_unit)
   endif

   deallocate(index)

end subroutine trace_report

subroutine trace_set(my_id, my_html, my_csv, my_url)

   implicit none

   integer, optional, intent(in) :: my_id
   logical, optional, intent(in) :: my_html, my_csv
   character(len=*), optional, intent(in) :: my_url

   if(present(my_id)) then
      myproc = my_id
   endif

   if(present(my_html)) then
      use_html = my_html
   endif

   if(present(my_csv)) then
      use_csv = my_csv
   endif

   if(present(my_url)) then
      documentation_url = my_url
   endif

end subroutine trace_set

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

subroutine trace_stop

   implicit none

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


end module module_trace

