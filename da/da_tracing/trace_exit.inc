subroutine trace_exit( Name, &             ! in
                       Message )           ! in, optional

!-----------------------------------------------------------------------
!--Purpose: Trace exit from subroutine
!-----------------------------------------------------------------------

   implicit none

   character (len=*), intent(in)           :: Name         ! subroutine name
   character (len=*), optional, intent(in) :: Message      ! text to trace

   integer                         :: IOStatus        ! I-O return code 
   integer                         :: n
   integer                         :: Caller

   real                            :: current_time, diff

   call trace_time(current_time)

   traceDepth=traceDepth-1

!=======================================================================
!--Check timing and maximum heap memory usage
!=======================================================================

   call da_diff_time(tracerStartTime(tracerID), current_time, diff)
   tracerCPUTime(tracerID) = tracerCPUTime(tracerID)  + diff

   call da_diff_time(tracerStartElapsedTime(tracerID), current_time, diff)
   tracerElapsedTime(tracerID) = tracerElapsedTime(tracerID)  + diff

!--Reset tracerID to its parent.
   tracerID = tracerParentID(tracerID)

   call trace_time(traceCurrentTime)
   tracerStartTime(tracerID) = traceCurrentTime

   call trace_diff_time(current_time, traceCurrentTime, diff)
   traceUsedTime = traceUsedTime + diff

end subroutine trace_exit

