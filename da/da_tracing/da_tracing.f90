MODULE da_tracing

   use da_constants
   use module_wrf_error
   use da_par_util1
   use da_reporting


   INTEGER, PARAMETER :: TraceIndentAmount      = 2   ! default indent
   INTEGER, PARAMETER :: MaxNoRoutines          = 440 ! maxium number of subroutines
   INTEGER, PARAMETER :: TraceNameLen           = 31  ! Length of trace name

   ! Variables

   INTEGER :: TraceDepth                   ! Current depth of trace
   INTEGER :: NoRoutines                   ! Number of routines so far
   INTEGER :: NoCalls(MaxNoRoutines)       ! Number of calls to each routine
   INTEGER :: NoCallsBody(MaxNoRoutines)   ! Number of calls in body of each routine
   INTEGER :: CalledBy(MaxNoRoutines)
   INTEGER :: MaxHeap(MaxNoRoutines)
   INTEGER :: EntryHeap(MaxNoRoutines)
   INTEGER :: Pointer                      ! pointer to routine arrays in TIMER.
   INTEGER :: BaseElapsedTime
   REAL :: BaseCPUTime
   INTEGER :: LastSpace
   INTEGER :: nproc, mype

   ! All CPU times in seconds

   REAL    :: CPUTimeStart(MaxNoRoutines)
   REAL    :: CPUTimeLocalStart
   REAL    :: CPUTime(MaxNoRoutines)
   REAL    :: CPUTimeLocal(MaxNoRoutines)
   REAL    :: CPUTimeThisCall(MaxNoRoutines)

   ! All Elapsed times based on wall clock in seconds

   REAL    :: ElapsedTimeStart(MaxNoRoutines)
   REAL    :: ElapsedTimeLocalStart
   REAL    :: ElapsedTime(MaxNoRoutines)
   REAL    :: ElapsedTimeLocal(MaxNoRoutines)
   REAL    :: ElapsedTimeThisCall(MaxNoRoutines)

   LOGICAL :: TraceActive = .FALSE.        ! Is it active in this routine?

   CHARACTER (LEN=TraceNameLen) :: TraceStartedBy  ! Subroutine name 
                                                   ! that activated trace
   CHARACTER (LEN=TraceNameLen) :: TimerNames(MaxNoRoutines) ! Subroutine names
   CHARACTER (LEN=TraceNameLen) :: TraceNames(MaxNoRoutines) ! for timing and tracing

   LOGICAL :: trace_write = .FALSE.


CONTAINS

#include "da_trace_init.inc"
#include "da_trace_entry.inc"
#include "da_trace.inc"
#include "da_trace_exit.inc"
#include "da_trace_int_sort.inc"
#include "da_trace_real_sort.inc"
#include "da_trace_report.inc"


END module da_tracing
