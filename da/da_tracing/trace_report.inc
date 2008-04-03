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

   integer :: IOStatus             ! I/O return code
   integer :: Loop1
   character (len=200) :: TraceFile
   character (len=200) :: csvname
   character (len=10)  :: temp

   call time(current_time)

   call da_diff_time(traceBaseTime, current_time, TotalElapsedTime)
   call da_diff_time(traceBaseTime, current_time, TotalCPUTime)

!==========================================================================
!--Sort subroutines into time order based on local Elapsed Time.
!--All PEs should have the same sort order after the sum.
!==========================================================================

   allocate (index(totalRoutine))

   call trace_real_sort(tracerElapsedTime, totalRoutine, index)

!-----------------------------------------------------------------
!--Open trace output file. 
!-----------------------------------------------------------------

   if (trace_write .AND. trace_unit /= 0) then
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
!          access="append", &      ! i:
           iostat=IOStatus)        ! O:

      if (IOStatus /= 0) then
         call trace_error(__FILE__,__LINE__, &
            (/"Cannot open trace file "//traceFile/))
      endif
   endif

   if(use_csv) then
      write(unit=csvname,fmt='(I10,A)') myproc,'.csv'
      open(unit=trace_csv_unit,file="trace/"//trim(adjustl(csvname)), &
         status="unknown",iostat=IOStatus)
      if (IOStatus /= 0) then
         call trace_error(__FILE__,__LINE__,(/"Cannot open "//csvname/))
      endif
   endif

   if(trace_write) then
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
   endif

   write(unit=trace_unit, fmt='(A,I4,A)') &
      "</pre><hr><H1>For PE",myproc,"</H1>"

!--Output timing information

   write(unit=trace_unit, fmt='(a)') &
        "<a name=local><h2>Local Timing Summary</h2></a>"

   write(unit=trace_unit, fmt='(a, f8.1, a)') &
        "(Tracing itself took ", traceUsedTime, &
        "s CPU Time. This is not included in the times below.)<p>"

   write(unit=trace_unit, fmt='(a)') &
        "<TABLE BORDER>", &
        "<TR>", &
        "<TH>Routine Name<TH>Calls<TH>Elapsed Time (seconds)<TH>percentElapsed<TH>CPU Time (seconds)<TH>percentCPU", &
        "</TR>"

   do i=totalRoutine,1,-1
      current_tracer=index(i)    

      if(totalCPUTime > 0.0) then
         percentCPUTime=100.0 * tracerCPUTime(current_tracer)/totalCPUTime
      else
         percentCPUTime=100.0
      end if

      if(totalElapsedTime > 0.0) then
         percentElapsedTime=100.0*tracerElapsedTime(current_tracer)/totalElapsedTime
      else
         percentElapsedTime=100.0
      end if

!------------------------------------------------------------------------
!-----Write out results. Note the abnormally long format line is needed as
!-----the NAG compiler complains if a quoted line is split.
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

      write(unit=trace_unit, fmt='(a,i10,10(a,f8.2))') &
            "<TD>", tracerBeingCalled(current_tracer), &
            "<TD>", tracerElapsedTime(current_tracer), &
            "<TD>", percentElapsedTime, &
            "<TD>", tracerCPUTime(current_tracer), &
            "<TD>", percentCPUTime

      if(use_csv) then
         write(unit=trace_csv_unit, fmt='(a,2x,2a,2x,i8,10(a,f8.2))') &
              'local,', &
              trim(subroutineNames(current_tracer)), &
              ',', tracerBeingCalled(current_tracer), &
              ",", tracerElapsedTime(current_tracer), &
              ",", percentElapsedTime, &
              ",", tracerCPUTime(current_tracer), &
              ",", percentCPUTime
      endif
   enddo

   write(unit=trace_unit, fmt='(a,i4,a,f8.1,a,f8.1,a)') &
      "<TR><TD><B>Total</B></TD><TD>",totalRoutine, "</TD><TD><B>", &
      totalElapsedTime, &
      "</B></TD><TD></TD><TD><B>", &
      TotalCPUTime, &
      "</B></TD><TD></TD>"

   write(unit=trace_unit, fmt='(a)') &
        "</TABLE><p><p>"

   if(use_csv) then
      write(unit=trace_csv_unit, fmt='(a)') " "
   end if

!--===================================================================================
!-- Sort subroutines into time order based on overall Elapsed Time.
!-- All PEs should have the same sort order after the sum. 
!--===================================================================================

   call trace_real_sort(tracerCPUTime,totalRoutine,index)

!-- Output timing information

   write(unit=trace_unit, &
      fmt='("</pre><hr><a name=overall><h2>Timing Summary</h2></a>")')

   write(unit=trace_unit, fmt='(a, f8.1, a)') &
        "(Tracing itself took ", traceUsedTime, &
        "s CPU Time. This is not included in the times below.)<p>"

   write(unit=trace_unit, fmt='(a)') &
        "<TABLE BORDER>", &
        "<TR>", &
        "<TH>Routine Name<TH>Calls<TH>Elapsed Time (seconds)<TH>percentElapsed<TH>CPU Time (seconds)<TH>percentCPU", &
        "</TR>"

   do i=totalRoutine,1,-1
      current_tracer=index(i)    

      if(totalCPUTime > 0.0) then
         percentCPUTime=100.0 * tracerCPUTime(current_tracer)/totalCPUTime
      else
         percentCPUTime=100.0
      end if

      if(totalElapsedTime > 0.0) then
         percentElapsedTime=100.0*tracerElapsedTime(current_tracer)/totalElapsedTime
      else
         percentElapsedTime=100.0
      end if

!------------------------------------------------------------------------
!-----Write out results. Note the abnormally long format line is needed as
!-----the NAG compiler complains if a quoted line is split.
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

      write(unit=trace_unit, fmt='(a,i10,10(a,f8.2))') &
            "<TD>", tracerBeingCalled(current_tracer), &
            "<TD>", tracerElapsedTime(current_tracer), &
            "<TD>", percentElapsedTime, &
            "<TD>", tracerCPUTime(current_tracer), &
            "<TD>", percentCPUTime

      if(use_csv) then
         write(unit=trace_csv_unit, fmt='(a,2x,2a,2x,i8,10(a,f8.2))') &
              'local,', &
              trim(subroutineNames(current_tracer)), &
              ',', tracerBeingCalled(current_tracer), &
              ",", tracerElapsedTime(current_tracer), &
              ",", percentElapsedTime, &
              ",", tracerCPUTime(current_tracer), &
              ",", percentCPUTime
      endif
   enddo

   write(unit=trace_unit, fmt='(a,i4,a,f8.1,a,f8.1,a)') &
      "<TR><TD><B>Total</B></TD><TD>",totalRoutine, "</TD><TD><B>", &
      totalElapsedTime, &
      "</B></TD><TD></TD><TD><B>", &
      TotalCPUTime, &
      "</B></TD><TD></TD>"

   write(unit=trace_unit, fmt='(a)') &
        "</TABLE><p><p>"

   if(use_csv) then
      write(unit=trace_csv_unit, fmt='(a)') " "
   endif

   if(trace_unit /= 0) then
      close(trace_unit)
   endif
  
   if(use_csv) then
      close(trace_csv_unit)
   endif

   deallocate(index)

end subroutine trace_report

