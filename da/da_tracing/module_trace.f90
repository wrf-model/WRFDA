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
   character(LEN= 8) :: traceStartDate, traceEndDate
   character(LEN=10) :: traceStartTime, traceEndTime

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

   logical :: trace_write = .false.
   logical :: NewTraceRoutine = .true.

contains

#include "trace_diff_time.inc"
#include "trace_entry.inc"
#include "trace_error.inc"
#include "trace_exit.inc"
#include "trace_info.inc"
#include "trace_int_sort.inc"
!include "trace_init.inc"
#include "trace_real_sort.inc"
#include "trace_report.inc"
#include "trace_start.inc"
#include "trace_time.inc"

end module module_trace

