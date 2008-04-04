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
      call trace_error(__FILE__,__LINE__, &
         (/"Cannot open trace file "//traceFile/))
   endif

   if(use_csv) then
      write(unit=csvname,fmt='(I10,A)') myproc,'.csv'
      open(unit=trace_csv_unit,file="trace/"//trim(adjustl(csvname)), &
         status="unknown",iostat=IOStatus)
      if (IOStatus /= 0) then
         call trace_error(__FILE__,__LINE__,(/"Cannot open "//csvname/))
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

