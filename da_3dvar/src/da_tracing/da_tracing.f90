module da_tracing

   use da_constants
#ifdef DM_PARALLEL
   use par_util
   include "mpif.h"
#endif


   integer, parameter :: TraceIndentAmount      = 2   ! default indent
   integer, parameter :: MaxNoRoutines          = 440 ! maxium number of subroutines
   integer, parameter :: TraceNameLen           = 31  ! Length of trace name

   character (LEN=*), parameter :: &
      pad = "                                                                "


   ! Variables

   integer :: TraceDepth                   ! Current depth of trace
   integer :: NoRoutines                   ! Number of routines so far
   integer :: NoCalls(MaxNoRoutines)       ! Number of calls to each routine
   integer :: NoCallsBody(MaxNoRoutines)   ! Number of calls in body of each routine
   integer :: CalledBy(MaxNoRoutines)
   integer :: MaxHeap(MaxNoRoutines)
   integer :: EntryHeap(MaxNoRoutines)
   integer :: Pointer                      ! pointer to routine arrays in TIMER.
   integer :: BaseElapsedTime
   real :: BaseCPUTime
   integer :: LastSpace

   ! All CPU times in seconds

   real    :: CPUTimeStart(MaxNoRoutines)
   real    :: CPUTimeLocalStart
   real    :: CPUTime(MaxNoRoutines)
   real    :: CPUTimeLocal(MaxNoRoutines)
   real    :: CPUTimeThisCall(MaxNoRoutines)

   ! All Elapsed times based on wall clock in seconds

   real    :: ElapsedTimeStart(MaxNoRoutines)
   real    :: ElapsedTimeLocalStart
   real    :: ElapsedTime(MaxNoRoutines)
   real    :: ElapsedTimeLocal(MaxNoRoutines)
   real    :: ElapsedTimeThisCall(MaxNoRoutines)

   logical :: TraceActive = .false.        ! Is it active in this routine?

   character (LEN=TraceNameLen) :: TraceStartedBy  ! Subroutine name 
                                                   ! that activated trace
   character (LEN=TraceNameLen) :: TimerNames(MaxNoRoutines) ! Subroutine names
   character (LEN=TraceNameLen) :: TraceNames(MaxNoRoutines) ! for timing and tracing

   logical :: trace_write = .false.


contains

#include "da_trace_init.inc"
#include "da_trace_entry.inc"
#include "da_trace.inc"
#include "da_trace_exit.inc"
#include "da_trace_int_sort.inc"
#include "da_trace_real_sort.inc"
#include "da_trace_report.inc"


end module da_tracing
