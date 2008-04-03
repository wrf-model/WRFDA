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

   write(unit=0, fmt='(3a,i4,a,f12.2)') &
        'enter ', trim(name), ' as id:', tracerID, ', at: ', current_time

!--stop previd timer
   call trace_diff_time(tracerStartTime(previd), current_time, diff)
   tracerCPUTime(previd) = tracerCPUTime(previd) + diff

!--Remember its parent ID.
   tracerParentID(tracerID) = previd

!--Increase the number of current routine being called.
   tracerBeingCalled(tracerID) = tracerBeingCalled(tracerID) + 1

!--Just start record the time
   tracerStartTime(tracerID) = current_time

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

