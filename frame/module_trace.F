module module_trace

#ifdef DM_PARALLEL
   use mpi, only : mpi_character, mpi_integer, mpi_sum, mpi_real8
#endif

   use module_driver_constants, only : max_domains, max_moves

   interface
      ! c code
      subroutine memory_info(memory_used)
         integer, intent(out) :: memory_used
      end subroutine memory_info
   end interface

   integer, parameter :: TraceIndentAmount      = 2   ! default indent
   integer, parameter :: MaxNoRoutines          = 440 ! maxium number of subroutines
   integer, parameter :: TraceNameLen           = 31  ! Length of trace name
   integer, parameter :: trace_csv_unit = 8
   integer :: trace_start_points=0   ! Number of routines to initiate trace

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
   logical, external :: rootproc
   integer :: local_communicator, mytask, ntasks, ierr, root

! define as temporaries
#include <namelist_defines.inc>

contains

subroutine trace_init

   implicit none
#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

   !--------------------------------------------------------------------
   ! Purpose: Initialise tracing
   !--------------------------------------------------------------------

   integer :: IOStatus             ! I/O return code
   integer :: Loop1
   character (len=200) :: TraceFile
   character(len=200) :: csvname
   character (len=10)  :: temp

#ifdef DM_PARALLEL
   CALL mpi_comm_rank( mpi_comm_world, mytask, ierr )
   if (rootproc()) then
      root = mytask
   endif
#else
   root = 0
#endif

! Statements that set the namelist vars to default vals
#  include <namelist_defaults.inc>

   IOStatus = 0

   if (trace_all_pes .OR. mytask == root) then
     trace_write = .true.  
   end if

   !-----------------------------------------------------------------
   ! Open trace output file. 
   !-----------------------------------------------------------------

   if (trace_write .AND. trace_unit /= 0) then
      if (use_html) then
         write(unit=temp,fmt='(I10)') mytask
         TraceFile="trace/"//trim(adjustl(temp))//".html"
         open (&
            unit=trace_unit, &   ! i:
            file=trim(tracefile), &   ! i:
            status="replace", & ! i:
            action="write", &   ! i:
            iostat=iostatus)    ! o:
      else   
         write(unit=temp,fmt='(i10)') mytask
         tracefile="trace/"//trim(adjustl(temp))//".txt"
         open (&
            unit=trace_unit, &   ! i:
            file=trim(tracefile), &   ! i:
            status="replace", & ! i:
            action="write", &   ! i:
            iostat=IOStatus)    ! O:
      end if

      if (IOStatus /= 0) then
         call trace_error("da_tracing/trace_init.inc",47, &
            (/"Cannot open trace file "//TraceFile/))
      end if
   end if

#ifdef DM_PARALLEL
   CALL wrf_get_dm_communicator (local_communicator)
   CALL mpi_comm_size( local_communicator, ntasks, ierr )
   CALL mpi_comm_rank( local_communicator, mytask, ierr )
#else
   ntasks = 1
   mytask = 0
#endif
   root = 0

   if (trace_csv .and. rootproc()) then
         write(unit=csvname,fmt='(I10,A)') mytask,'.csv'
      open(unit=trace_csv_unit,file="trace/"//trim(adjustl(csvname)), &
         status="replace",iostat=IOStatus)
      if (IOStatus /= 0) then
         call trace_error("da_tracing/trace_init.inc",57,(/"Cannot open "//csvname/))
      end if
   end if

   !-----------------------------------------------------------------
   ! Find out whether to trace memory usage. The Cray routine to check
   ! memory usage is very slow, so it is best to only switch on memory
   ! checking if actively required.
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! Set up timing and memory usage
   !-----------------------------------------------------------------

   do Loop1=1,MaxNoRoutines
      CPUTimeStart(Loop1)     = 0.0
      ElapsedTimeStart(Loop1) = 0.0
      ElapsedTime(Loop1)      = 0.0
      ElapsedTimeLocal(Loop1) = 0.0
      CPUTime(Loop1)          = 0.0
      CPUTimeLocal(Loop1)     = 0.0
      NoCalls(Loop1)          = 0
      NoCallsBody(Loop1)      = 0
      CalledBy(Loop1)         = 0
      MaxHeap(Loop1)          = 0
      TimerNames(Loop1)       = ""
   end do

   Pointer     = 0
   NoRoutines  = 0

   call system_clock(&
      COUNT=BaseElapsedTime)

   call cpu_time(BaseCPUTime)

   ! start trace output here so memory calculations are not distorted
   ! by IO buffer being grabbed later

   TraceDepth = 0

   if (trace_write) then
      if (use_html) then
         write (unit=trace_unit,fmt='(A)') "<html><head><title>Tracing</title></head>"
         write (unit=trace_unit,fmt='(A)') "<body><h1>Trace Output</h1>"
         write (unit=trace_unit,fmt='(A)') "<ul>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#tree>Calling Tree</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#local>Local routine timings</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#overall>Overall routine timings</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#memory>Memory usage</a>"
         write (unit=trace_unit,fmt='(A)') "</ul>"
         write (unit=trace_unit,fmt='(A)') "<a name=tree><h2>Calling Tree</h2></a><pre>"
      else
         write (unit=trace_unit,fmt='(A)') "Trace Output"
         write (unit=trace_unit,fmt='(A)') ""
      end if
   end if

end subroutine trace_init


subroutine trace_entry(&
   Name, &                       ! in
   Message, &                    ! in, optional
   Messages, &                   ! in, optional
   MaxNoCalls)                   ! in, optional

   !-----------------------------------------------------------------------
   ! Purpose: Trace entry point to subroutine
   !-----------------------------------------------------------------------

   implicit none

   character (len=*),           intent(in) :: Name         ! Routine name
   character (len=*), optional, intent(in) :: Message      ! message
   character (len=*), optional, intent(in) :: Messages(:)  ! message array
   integer, optional,           intent(in) :: MaxNoCalls   ! max no calls to show


   integer           :: IOStatus        ! I-O return code
   integer           :: Loop            ! General loop counter
   integer           :: Count
   integer           :: OldPointer
   integer           :: TotalSpace
   integer           :: LocalMaxNoCalls
   real              :: CPUTime1
   real              :: temp1
   real              :: temp2
   logical           :: NewRoutine

   call cpu_time(CPUTime1)

   call system_clock(&
      COUNT=Count)

   !-----------------------------------------------------------------------
   ! check if tracing active. If not check whether to switch it on
   !-----------------------------------------------------------------------

   if (.NOT. TraceActive) then
      if (trace_start_points == 0) then
         ! start with first call
         TraceActive = .true.
      else
         do Loop=1,trace_start_points
            if (Name == TraceNames(Loop)(1:LEN(Name))) then
               TraceActive    = .true.
               TraceDepth     = 0
               TraceStartedBy = Name
               exit
            end if
         end do
      end if
      if (.NOT. TraceActive) then
         ! did not want to start trace, so leave
         return
      end if
   end if

   !-----------------------------------------------------------------------
   ! timing and maximum heap usage
   !-----------------------------------------------------------------------

   ! Increment the local elapsed time and local CPU time since the
   ! last trace entry, if any

   if (Pointer /= 0) then
      temp1 = real(Count - BaseElapsedTime) - ElapsedTimeLocalStart
      temp2 = CPUTime1 - CPUTimeLocalStart
      ElapsedTimeLocal(Pointer)    = ElapsedTimeLocal(Pointer) + temp1
      ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
      CPUTimeLocal(Pointer)        = CPUTimeLocal(Pointer) + temp2
      CPUTimeThisCall(Pointer)     = CPUTimeThisCall(Pointer) + temp2
   end if

   OldPointer=Pointer

   ! Check subroutine name 

   NewRoutine = .true.
   do Pointer=1,NoRoutines     
      if (TimerNames(Pointer) == Name) then
         NewRoutine=.false.
         exit
      end if
   end do

   if (NewRoutine) then
      ! New subroutine entered
      if (NoRoutines >= MaxNoRoutines)then ! too many to trace
          call trace_error("da_tracing/trace_entry.inc",90, &
             (/"Too many routines. Not timing " // Name/))

         !All the timings etc are put instead to the calling routine,
         ! which therefore may have incorrect summaries.
         !The best solution is to increase MaxNoRoutines.
         Pointer = OldPointer
         ! Fix to get the correct NoCalls(OldPointer) despite the +1 later
         NoCalls(Pointer)=NoCalls(Pointer)-1

      else ! Pointer=NoRoutines+1 (from the end of earlier do loop)
         NoRoutines=NoRoutines+1
         TimerNames(NoRoutines)=Name
      end if
   end if

   NoCalls(Pointer)=NoCalls(Pointer)+1

   CPUTimeThisCall(Pointer)     = 0.0
   ElapsedTimeThisCall(Pointer) = 0.0

   CalledBy(Pointer)=OldPointer

   if (trace_memory) then
      call memory_info(&
         TotalSpace)
      EntryHeap(Pointer) = TotalSpace
      LastSpace = TotalSpace
      if (MaxHeap(Pointer) < TotalSpace) then
         MaxHeap(Pointer) = TotalSpace
      end if
   else
      TotalSpace = 0
   end if

   if (trace_write .AND. TraceDepth <= trace_max_depth) then

      if (present(MaxNoCalls)) then
         LocalMaxNoCalls = MaxNoCalls
      else
         LocalMaxNoCalls = trace_repeat_head
      end if

      if (NoCalls(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>",I11)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(documentation_url), &
                  trim(Name),trim(Name), TotalSpace
            else
               write (unit=trace_unit, &
                  fmt='(A,"> ",A,I11)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace
           end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name)
            else
               write (unit=trace_unit, fmt='(A,"> ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name)
            end if
         end if
         if (IOStatus /= 0) then
            call trace_error("da_tracing/trace_entry.inc",160, &
               (/"Cannot write to trace file for "/))
         end if

         if (present(Message)) then
            write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),trim(Message)
            if (IOStatus .NE. 0) then
               call trace_error("da_tracing/trace_entry.inc",168, &
                  (/"Cannot write to trace file for "/))
            end if
         end if

         if (present(Messages)) then
            do Loop = 1, size(Messages)
               write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Messages(Loop))
               if (IOStatus .NE. 0) then
                  call trace_error("da_tracing/trace_entry.inc",178, &
                     (/"Cannot write to trace file for "/))
               end if
            end do ! Loop
         end if
      end if

   end if ! trace_write

   TraceDepth=TraceDepth+1

   call system_clock(&
      COUNT=Count)

   call cpu_time(CPUTime1)

   ! set the start elapsed and CPU time both locally and generally

   ElapsedTimeStart(Pointer) = real(Count-BaseElapsedTime)
   ElapsedTimeLocalStart     = real(Count-BaseElapsedTime)

   CPUTimeStart(Pointer) = CPUTime1
   CPUTimeLocalStart     = CPUTime1

   ! call flush(trace_unit)

   return
end subroutine trace_entry


subroutine trace(&
   Name,     &           ! in
   Message,  &           ! in, optional
   Messages,  &          ! in, optional
   MaxNoCalls)           ! in, optional

   implicit none

   !--------------------------------------------------------------------
   ! Purpose: General trace within a subroutine
   !--------------------------------------------------------------------

   character (len=*), intent(in)           :: Name         ! Subroutine name
   character (len=*), optional, intent(in) :: Message      ! Text to trace
   character (len=*), optional, intent(in) :: Messages(:)  ! Text to trace
   integer, optional, intent(in)           :: MaxNoCalls   ! max no calls to show

   integer           :: IOStatus     ! I-O return code
   integer           :: Loop         ! General loop counter
   integer           :: TotalSpace
   integer           :: LocalMaxNoCalls
   character(len=25) :: Change

   !-----------------------------------------------------------------------
   ! Check whether trace active and depth of trace
   !-----------------------------------------------------------------------

   if (.NOT. TraceActive) then
      return
   end if

   if (TraceDepth >= trace_max_depth) then
      ! already at maximum depth, so return
      return
   end if

   !-----------------------------------------------------------------------
   ! Note memory usage
   !-----------------------------------------------------------------------

   Change = ""

   if (trace_memory) then
      call memory_info(&
         TotalSpace)
      if (LastSpace < TotalSpace) then
         write(Change,"(A9,I12)")", bigger", TotalSpace - LastSpace
      else if (LastSpace > TotalSpace) then
         write(Change,"(A9,I12)")", smaller", TotalSpace - LastSpace
      end if
      if (MaxHeap(Pointer) < TotalSpace) then
         MaxHeap(Pointer) = TotalSpace
      end if
      LastSpace = TotalSpace
   else
      TotalSpace = 0
   end if

   !-----------------------------------------------------------------------
   ! Perform the trace if not done too many times before. only on PE 0
   !-----------------------------------------------------------------------  

   if (trace_write) then

      if (present(MaxNoCalls)) then
         LocalMaxNoCalls = MaxNoCalls
      else
         LocalMaxNoCalls = trace_repeat_body
      end if

      NoCallsBody(Pointer) = NoCallsBody(Pointer)+1

      if (NoCallsBody(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
             if (use_html) then
                write (unit=trace_unit, &
                   fmt='(A, "| <a href=",A,"/",A,".html>",A,"</a>",I11,A)', &
                   iostat=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                   trim(Name), trim(Name), TotalSpace, Change
             else
                write (unit=trace_unit, &
                   fmt='(A, "| ",A,I11,A)', &
                   iostat=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace, Change
            end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "| <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name), trim(Name)  
            else 
               write (unit=trace_unit, &
                  fmt='(A, "| ",A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name)
            end if
         end if
         if (IOStatus /= 0) then
            call trace_error("da_tracing/trace.inc",102, &
               (/"Cannot write to trace file for "/))
         end if

         if (present(Message)) then
            write (unit=trace_unit, fmt='(A,"  ",A)', iostat=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),trim(Message)
            if (IOStatus .NE. 0) then
               call trace_error("da_tracing/trace.inc",110, &
                  (/"Cannot write to trace file for "/))
            end if
         end if

         if (present(Messages)) then
            do Loop = 1, size(Messages)
               write (unit=trace_unit, fmt='(A,"  ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Messages(Loop))
               if (IOStatus .NE. 0) then
                  call trace_error("da_tracing/trace.inc",120, &
                     (/"Cannot write to trace file for "/))
               end if
            end do ! Loop
         end if
      end if

      if (NoCallsBody(Pointer) == trace_repeat_body) then
         write (unit=trace_unit, fmt='(A,"  Called enough, going quiet")', iostat=IOStatus) &
            pad(1:TraceDepth*TraceIndentAmount)
         if (IOStatus .NE. 0) then
            call trace_error("da_tracing/trace.inc",131, &
              (/"Cannot write to trace file for "/))
         end if
      end if
   end if ! trace_write

end subroutine trace


subroutine trace_exit(&
   Name, &               ! in
   Message, &            ! in, optional
   Messages, &           ! in, optional
   MaxNoCalls)           ! in, optional

   !-----------------------------------------------------------------------
   ! Purpose: Trace exit from subroutine
   !-----------------------------------------------------------------------

   implicit none

   character (len=*), intent(in)           :: Name         ! subroutine name
   character (len=*), optional, intent(in) :: Message      ! text to trace
   character (len=*), optional, intent(in) :: Messages(:)  ! text to trace
   integer, optional, intent(in)           :: MaxNoCalls   ! max no calls to show

   integer                         :: IOStatus        ! I-O return code 
   integer                         :: Loop            ! General loop counter
   integer                         :: Count
   integer                         :: TotalSpace
   integer                         :: LocalMaxNoCalls
   integer                         :: Caller
   real                            :: temp_CPUTime
   real                            :: temp1
   real                            :: temp2
   character(len=25)               :: Change

   call cpu_time(temp_CPUTime)

   call system_clock(&
      COUNT=Count)

   !======================================================================
   ! check whether trace active and whether depth exceeded
   !======================================================================

   if (.NOT. TraceActive) then
      return
   end if

   if (TraceActive) then
      ! was tracing enabled by this routine? If it was, disable it, to
      ! take affect after the trace line has been written
      if (Name == TraceStartedBy(1:LEN(Name))) then
         TraceActive = .false.
      end if
   end if

   temp1 = real(Count - BaseElapsedTime) - ElapsedTimeLocalStart
   temp2 = temp_CPUTime - CPUTimeLocalStart

   TraceDepth=TraceDepth-1

   if (TraceDepth < 0) then
      TraceDepth = 0
   end if

   !=======================================================================
   ! Check timing and maximum heap memory usage
   !=======================================================================

   ElapsedTimeLocal(Pointer)    = ElapsedTimeLocal(Pointer) + temp1
   ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
   ElapsedTime(Pointer)         = ElapsedTime(Pointer) + &
      ElapsedTimeThisCall(Pointer)

   CPUTimeLocal(Pointer)        = CPUTimeLocal(Pointer) + temp2
   CPUTimeThisCall(Pointer)     = CPUTimeThisCall(Pointer) + temp2
   CPUTime(Pointer)             = CPUTime(Pointer) + CPUTimeThisCall(Pointer)

   Caller=CalledBy(Pointer)
   if (Caller /= 0) then
      ElapsedTimeThisCall(Caller) = ElapsedTimeThisCall(Caller) + &
         ElapsedTimeThisCall(Pointer)
      CPUTimeThisCall(Caller) = CPUTimeThisCall(Caller) + CPUTimeThisCall(Pointer)
   end if

   Change = ""

   if (trace_memory) then
      call memory_info(&
         TotalSpace)
      if (EntryHeap(Pointer) < TotalSpace) then
         write(Change,"(A9,I12)")", BIGGER", TotalSpace - EntryHeap(Pointer)
      else if (EntryHeap(Pointer) > TotalSpace) then
         write(Change,"(A9,I12)")", SMALLER", TotalSpace - EntryHeap(Pointer)
      end if
      if (MaxHeap(Pointer) < TotalSpace) then
         MaxHeap(Pointer) = TotalSpace
      end if
   else
      TotalSpace = 0
   end if

   if (trace_write .AND. TraceDepth <= trace_max_depth) then

      if (present(MaxNoCalls)) then
         LocalMaxNoCalls = MaxNoCalls
      else
         LocalMaxNoCalls = trace_repeat_head
      end if

      IOStatus=0

      if (NoCalls(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "&lt; <a href=",A,"/",A,".html>",A,"</a>",I11,A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name), TotalSpace, Change
            else
               write (unit=trace_unit, &
                  fmt='(A, "< ",A,I11,A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace, Change
            end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "&lt; <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name)
            else
               write (unit=trace_unit, fmt='(A, "< ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name)
            end if
         end if

         if (IOStatus /= 0) then
            call trace_error("da_tracing/trace_exit.inc",134, &
              (/"Cannot write to trace file for "/))
         end if

         if (present(Message)) then
            write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),trim(Message)
            if (IOStatus .NE. 0) then
               call trace_error("da_tracing/trace_exit.inc",142, &
                  (/"Cannot write to trace file for "/))
            end if
         end if

         if (present(Messages)) then
            do Loop = 1, size(Messages)
               write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Messages(Loop))
               if (IOStatus .NE. 0) then
                  call trace_error("da_tracing/trace_exit.inc",152, &
                     (/"Cannot write to trace file for "/))
               end if
            end do ! Loop
         end if
      end if

      if (NoCalls(Pointer) == trace_repeat_head) then
         write(unit=trace_unit,fmt='(A,"  Called enough, going quiet")', &
            iostat=IOStatus)&
            pad(1:TraceDepth*TraceIndentAmount)
         if (IOStatus .NE. 0) then
            call trace_error("da_tracing/trace_exit.inc",164, &
               (/"Cannot write to trace file for "/))
         end if
      end if
   end if ! trace_write

   ! Restore pointer
   Pointer = CalledBy(Pointer)

   ! note local time

   call system_clock(&
     count=count)

   elapsedtimelocalstart = real(count-baseelapsedtime)
   call cpu_time(cputimelocalstart)

   ! call flush(trace_unit)

end subroutine trace_exit


subroutine trace_int_sort(&
   key, &
   n, &
   index)

   !----------------------------------------------------------------------
   ! Purpose: sort integers for tracing
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in)          :: n      ! The number of items to be sorted. 
   integer, intent(in)          :: key(:)
   integer, intent(out) :: index(:)

   integer :: head       ! heaps are tree structures: head and child refer
   integer :: child      ! to related items within the tree 
   integer :: i          
   integer :: dum        ! used to swap index items


   ! initialise index:
   do i=1,n
      index(i)=i
   end do 

   ! Do heapsort: Create the heap...
   makeheap : do i=n/2,1,-1
      head=i
      sift1 : do
         ! find the largest out of the head and its two children...
         child=head*2
         if (child>n) exit sift1
         if (child<n) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if
         ! if the head is the largest, then sift is done...
         if (key(index(head))>=key(index(child))) exit sift1
         ! otherwise swap to put the largest child at the head,
         ! and prepare to repeat the procedure for the head in its new
         ! subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift1
   end do makeheap

   ! Retire heads of the heap, which are the largest, and
   ! stack them at the end of the array.
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


subroutine trace_real_sort(&
   key, &
   n, &
   index)

   !-----------------------------------------------------------------------
   ! Purpose: Sort reals for tracing
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n      ! The number of items to be sorted. 
   real,    intent(in)  :: key(:)
   integer, intent(out) :: index(:)

   integer :: head       ! heaps are tree structures: head and child refer
   integer :: child      ! to related items within the tree 
   integer :: i          
   integer :: dum        ! used to swap index items

   ! initialise index:
   do i=1,n
      index(i)=i
   end do

   ! Do heapsort: Create the heap...
   makeheap : do i=n/2,1,-1
      head=i
      sift1 : do
         ! find the largest out of the head and its two children...
         child=head*2
         if (child>n) exit sift1
         if (child<n) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if
         ! if the head is the largest, then sift is done...
         if (key(index(head))>=key(index(child))) exit sift1
         ! otherwise swap to put the largest child at the head,
         ! and prepare to repeat the procedure for the head in its new
         ! subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift1
   end do makeheap

   ! Retire heads of the heap, which are the largest, and
   ! stack them at the end of the array.

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

end subroutine trace_real_sort


subroutine trace_report

   !--------------------------------------------------------------------
   ! Purpose: Produce a trace report
   !--------------------------------------------------------------------

   implicit none

   integer :: i                        ! loop counter
   integer :: j                        ! loop counter
   integer :: CountRate
   integer :: MasterNoRoutines
   integer :: temp
   integer :: MinElapsedPos
   integer :: MaxElapsedPos
   integer :: MinCPUPos
   integer :: MaxCPUPos
   integer :: itemp1(MaxNoRoutines)
   integer :: MasterMaxHeap(0:ntasks-1,MaxNoRoutines)
   integer :: MasterNoCalls(0:ntasks-1,MaxNoRoutines)
   integer :: OverallNoCalls(MaxNoRoutines)
   integer, allocatable :: Index(:)

   real    :: TempCpuTime
   real    :: TotalElapsedTime             !
   real    :: TotalCPUTime(1)              !
   real    :: SpeedUp                      ! speed up factor
   real    :: PercentCPUTime               ! percentage in CPU time
   real    :: PercentElapsedTime           ! percentage in elapsed time
   real    :: rtemp1(MaxNoRoutines)
   real    :: MasterElapsedTime(0:ntasks-1,MaxNoRoutines)
   real    :: MasterElapsedTimeLocal(0:ntasks-1,MaxNoRoutines)
   real    :: MasterCPUTime(0:ntasks-1,MaxNoRoutines)
   real    :: MasterCPUTimeLocal(0:ntasks-1,MaxNoRoutines)
   real    :: OverallElapsedTime(MaxNoRoutines)
   real    :: OverallCPUTime(MaxNoRoutines)

   character (len=TraceNameLen) :: MasterTimerNames(MaxNoRoutines)

   if (.not. use_html) then
      write (unit=trace_unit, fmt='(A)') &
         "Report only available if use_html is true"
      return
   end if

   call system_clock (COUNT=temp)

   TotalElapsedTime=temp-BaseElapsedTime ! on PE 0

   call cpu_time(TempCpuTime)

   TotalCPUTime(1) = TempCpuTime - BaseCPUTime

   call system_clock(&
      COUNT_RATE=CountRate)

   ! Ensure the lists from each PE match. use the routine list from the 
   ! traced PE as the master copy

   MasterTimerNames(:)=TimerNames(:)

   if (mytask == trace_pe) then
      MasterNoRoutines=NoRoutines
   else
      MasterNoRoutines=0
   end if

   call da_proc_sum_int(MasterNoRoutines)
   ! only PE 0 knows the result

#ifdef DM_PARALLEL
   call mpi_bcast(MasterTimerNames(1:MaxNoRoutines), &
                  TraceNameLen*MaxNoRoutines, &
                  MPI_character,trace_pe, local_communicator,ierr)
#endif

   MasterElapsedTime(:,:)=0.0
   MasterCPUTime(:,:)=0.0
   MasterElapsedTimeLocal(:,:)=0.0
   MasterCPUTimeLocal(:,:)=0.0
   MasterNoCalls(:,:)=0
   MasterMaxHeap(:,:)=0

   do i=1,MaxNoRoutines
      do j=1,NoRoutines
         if (TimerNames(j) == MasterTimerNames(i)) then
            MasterElapsedTime(mytask,i)=ElapsedTime(j)
            MasterCPUTime(mytask,i)=CPUTime(j)
            MasterElapsedTimeLocal(mytask,i)=ElapsedTimeLocal(j)
            MasterCPUTimeLocal(mytask,i)=CPUTimeLocal(j)
            MasterNoCalls(mytask,i)=NoCalls(j)
            MasterMaxHeap(mytask,i)=MaxHeap(j)
            cycle
         end if
      end do
   end do

   do i=0,ntasks-1
      call da_proc_sum_real(MasterElapsedTime(i,:))
      call da_proc_sum_real(MasterCPUTime(i,:))
      call da_proc_sum_real(MasterElapsedTimeLocal(i,:))
      call da_proc_sum_real(MasterCPUTimeLocal(i,:))
      call da_proc_sum_ints(MasterNoCalls(i,:))
      call da_proc_sum_ints(MasterMaxHeap(i,:))
   end do

   if (rootproc()) then

      do j=1,MasterNoRoutines
         rtemp1(j)=sum(MasterElapsedTimeLocal(:,j))
      end do
      !==========================================================================
      ! Sort subroutines into time order based on local Elapsed Time.
      ! All PEs should have the same sort order after the sum.
      !==========================================================================

      allocate (Index(MasterNoRoutines))

      call trace_real_sort(rtemp1,MasterNoRoutines,index)

      do j=1,MasterNoRoutines
         OverallElapsedTime(j)=sum(MasterElapsedTimeLocal(:,j))
         OverallCPUTime(j)=sum(MasterCPUTimeLocal(:,j))
         OverallNoCalls(j)=sum(MasterNoCalls(:,j))
      end do

      write(unit=trace_unit, fmt='(A,I4,A)') &
         "</pre><hr><H1>For PE",trace_pe,"</H1>"

      ! Output timing information

      write(unit=trace_unit, &
         fmt='("<a name=local><h2>Local Timing Summary</h2></a>")')

      if (ntasks == 1) then
         ! needs changing to work MPP
         write (unit=trace_unit, &
            fmt='("(Tracing itself took ",F8.1,"s CPU Time, and ",F8.1,a)') &
            (CPUTimeLocalStart-CPUTimeStart(1)-TotalCPUTime(1)), &
            (ElapsedTimeLocalStart-ElapsedTimeStart(1)-TotalElapsedTime) / &
            real(CountRate), &
            "s elapsed time during the run. This is not included in the times below.)<p>"
      else
         write (unit=trace_unit,fmt='(A)') &
            "No estimate can be made of time in trace itself.<p>"
      end if

      write(unit=trace_unit, &
         fmt='("<TABLE BORDER>")')
      write(unit=trace_unit, &
         fmt='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      write(unit=trace_unit, &
         fmt='("CPU Time (seconds)<TH>Speed up")')
      write(unit=trace_unit, &
         fmt='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum <TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      write(unit=trace_unit, &
         fmt='("<TH>",I4," PE")') ntasks

      do i=MasterNoRoutines,1,-1
         Pointer=index(i)    

         if (TotalCPUTime(1) > 0.0) then
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         else
           PercentCPUTime=100.0
         end if

         if (TotalElapsedTime > 0.0) then
            PercentElapsedTime=100.0*OverallElapsedTime(Pointer)/(real(ntasks) * TotalElapsedTime)
         else
            PercentElapsedTime=100.0
         end if

         if (OverallElapsedTime(Pointer) > 0.0) then
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/real(CountRate*ntasks))
         else
            SpeedUp = 0.0
         end if

         ! This horrible solution as MinLOC does not take a DIM argument, so sum
         ! is needed to convert the array to an integer

         MinElapsedPos = sum(MinLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MaxElapsedPos = sum(MAXLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MinCPUPos     = sum(MinLOC(MasterCPUTimeLocal(:,Pointer)))-1
         MaxCPUPos     = sum(MAXLOC(MasterCPUTimeLocal(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         write (unit=trace_unit, fmt='(7A)') &
            "<TR><TD><a href=", &
            trim(Documentation_url), &
            "/", &
            trim(MasterTimerNames(Pointer)), & ! Subroutine name
            ".html>", &
            trim(MasterTimerNames(Pointer)), & ! Subroutine name
            "</a>"
         write (unit=trace_unit, &
            fmt='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            real(OverallNoCalls(Pointer))/real(ntasks),                       & ! Average number of calls per PE
            OverallElapsedTime(Pointer)/(ntasks*real(CountRate)),             & ! Average Elapsed Time
            PercentElapsedTime,                                              & ! Percent Elapsed Time
            MasterElapsedTimeLocal(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                                   & ! Which PE
            MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
            MaxElapsedPos,                                                   & ! Which PE
            OverallCPUTime(Pointer),                                         & ! CPU time
            PercentCPUTime,                                                  & ! Percent CPU time
            MasterCPUTimeLocal(MinCPUPos,Pointer),                           & ! Min average CPU Time
            MinCPUPos,                                                       & ! Which PE
            MasterCPUTimeLocal(MaxCPUPos,Pointer),                           & ! Max average CPU Time
            MaxCPUPos,                                                       & ! Which PE
            SpeedUp                                                            ! Speedup
         if (trace_csv) then
            write(unit=trace_csv_unit,  &
               fmt='(2A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"local",', &
               '"'//trim(MasterTimerNames(Pointer))//'",', &
               real(OverallNoCalls(Pointer))/real(ntasks),                       & ! Average number of calls per PE
               OverallElapsedTime(Pointer)/(ntasks*real(CountRate)),             & ! Average Elapsed Time
               PercentElapsedTime,                                              & ! Percent Elapsed Time
               MasterElapsedTimeLocal(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                                   & ! Which PE
               MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                                   & ! Which PE
               OverallCPUTime(Pointer),                                         & ! CPU time
               PercentCPUTime,                                                  & ! Percent CPU time
               MasterCPUTimeLocal(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                       & ! Which PE
               MasterCPUTimeLocal(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                       & ! Which PE
               SpeedUp                                                            ! Speedup
         end if
      end do

      write (unit=trace_unit, &
         fmt='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "<TD></TD><TD><B>", &
         TotalElapsedTime/real(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>", &
         TotalCPUTime(1),"</B><TD></TD><TD></TD><TD></TD>"
      if (TotalElapsedTime > 0.0) then
         write (unit=trace_unit, fmt='("<TD><B>",F8.1,"</B>")') &
            (TotalCPUTime(1))/(TotalElapsedTime/real(CountRate))
      end if
      write(unit=trace_unit, &
         fmt='("</TABLE><p><p>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,fmt=*) " "
      end if

      !===================================================================================
      ! Sort subroutines into time order based on overall Elapsed Time.
      ! All PEs should have the same sort order after the sum. 
      !===================================================================================

      do j=1,MasterNoRoutines
         rtemp1(j)=sum(MasterElapsedTime(:,j))
      end do

      call trace_real_sort(rtemp1,MasterNoRoutines,index)

      do j=1,MasterNoRoutines
         OverallElapsedTime(j)=sum(MasterElapsedTime(:,j))
         OverallCPUTime(j)=sum(MasterCPUTime(:,j))
      end do

      ! Output timing information

      write(unit=trace_unit, &
         fmt='("</pre><hr><a name=overall><h2>Overall Timing Summary</h2></a>")')

      write(unit=trace_unit, &
         fmt='("<TABLE BORDER>")')
      write(unit=trace_unit, &
         fmt='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      write(unit=trace_unit, &
         fmt='("CPU Time (seconds)<TH>Speed up")')
      write(unit=trace_unit, &
         fmt='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum<TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      write(unit=trace_unit, &
         fmt='("<TH>",I4," PE")') ntasks

      do i=MasterNoRoutines,1,-1
         Pointer=index(i)    

         if (TotalCPUTime(1) > 0.0) then
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         else
            PercentCPUTime=100.0
         end if

         if (TotalElapsedTime > 0.0) then
            PercentElapsedTime=100.0 * OverallElapsedTime(Pointer)/(real(ntasks) * TotalElapsedTime)
         else
            PercentElapsedTime=100.0
         end if

         if (OverallElapsedTime(Pointer) > 0.0) then
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/real(ntasks*CountRate))
         else
            SpeedUp = 0.0
         end if

         ! This horrible solution as MinLOC does not take a DIM argument, so 
         ! sum is needed to convert the array to an integer

         MinElapsedPos = sum(MinLOC(MasterElapsedTime(:,Pointer)))-1
         MaxElapsedPos = sum(MAXLOC(MasterElapsedTime(:,Pointer)))-1
         MinCPUPos     = sum(MinLOC(MasterCPUTime(:,Pointer)))-1
         MaxCPUPos     = sum(MaxLOC(MasterCPUTime(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         write (unit=trace_unit, fmt='(7A)') &
            "<TR><TD><a href=", &
            trim(Documentation_url), &
            "/", &
            trim(MasterTimerNames(Pointer)), &    ! Subroutine name
            ".html>", &
            trim(MasterTimerNames(Pointer)), &    ! Subroutine name
            "</a>"
         write (unit=trace_unit, &
            fmt='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            real(OverallNoCalls(Pointer))/real(ntasks),                  & ! Number of calls per PE
            OverallElapsedTime(Pointer)/(real(ntasks*CountRate)),        & ! Average Elapsed Time
            PercentElapsedTime,                                         & ! Percent Elapsed Time
            MasterElapsedTime(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                              & ! Which PE
            MasterElapsedTime(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
            MaxElapsedPos,                                              & ! Which PE
            OverallCPUTime(Pointer),                                    & ! CPU time
            PercentCPUTime,                                             & ! Percent CPU time
            MasterCPUTime(MinCPUPos,Pointer),                           & ! Min average CPU Time
            MinCPUPos,                                                  & ! Which PE
            MasterCPUTime(MaxCPUPos,Pointer),                           & ! Max average CPU Time
            MaxCPUPos,                                                  & ! Which PE
            SpeedUp                                                       ! SpeedUp
         if (trace_csv) then
            write (unit=trace_csv_unit, &
               fmt='(2A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"overall",', &
               '"'//trim(MasterTimerNames(Pointer))//'",', &
               real(OverallNoCalls(Pointer))/real(ntasks),                  & ! Number of calls per PE
               OverallElapsedTime(Pointer)/(real(ntasks*CountRate)),        & ! Average Elapsed Time
               PercentElapsedTime,                                         & ! Percent Elapsed Time
               MasterElapsedTime(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                              & ! Which PE
               MasterElapsedTime(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                              & ! Which PE
               OverallCPUTime(Pointer),                                    & ! CPU time
               PercentCPUTime,                                             & ! Percent CPU time
               MasterCPUTime(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                  & ! Which PE
               MasterCPUTime(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                  & ! Which PE
               SpeedUp                                                       ! SpeedUp
         end if
      end do

      write (unit=trace_unit, &
         fmt='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "</TD><TD><TD><B>", &
         TotalElapsedTime/real(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>",TotalCPUTime(1), &
         "</B><TD></TD><TD></TD><TD></TD>"
      if (TotalElapsedTime > 0.0) then
         write (unit=trace_unit, fmt='("<TD><B>",F8.1,"</B>")') &
            TotalCPUTime(1)/(TotalElapsedTime/real(CountRate))
      end if

      write(unit=trace_unit, &
         fmt='("</TABLE>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,fmt=*) " "
      end if

   end if ! rootproc

   !============================================================================
   ! Sort subroutines into memory use order by max memory on one PE
   !============================================================================

   if (trace_memory) then

      do j=1,MaxNoRoutines
         call da_proc_sum_ints(MasterMaxHeap(:,j))
      end do

      if (rootproc()) then
         do j=1,MasterNoRoutines
            itemp1(j)=MAXVAL(MasterMaxHeap(:,j))
         end do

         call trace_int_sort(itemp1,MasterNoRoutines,index)

         write (unit=trace_unit,fmt='("<hr><a name=memory><h2>Maximum memory usage for routines</h2></a>")')
         write (unit=trace_unit,fmt='("<TABLE BORDER>")')
         write (unit=trace_unit,fmt='("<TR><TH>Routine<TH>Max in any PE (kbytes)")')
         write (unit=trace_unit,fmt='("<TH>Overall (kbytes)<TH>Average per PE (kbytes)")')

         do i=MasterNoRoutines,1,-1
            Pointer=index(i)
            write (unit=trace_unit, &
               fmt='("<TR><TD><a href=",A,"/",A,".html>",A,"</a><TD>",I15,"<TD>",I15,"<TD>",I15)') &
               trim(Documentation_url),trim(TimerNames(Pointer)),trim(TimerNames(Pointer)), &
               MAXVAL(MasterMaxHeap(:,Pointer)),sum(MasterMaxHeap(:,Pointer)), &
               sum(MasterMaxHeap(:,Pointer))/ntasks
            if (trace_csv) then
               write (unit=trace_csv_unit, &
                  fmt='(2A,I15,",",I15,",",I15)') &
                  '"memory",', &
                  '"'//trim(TimerNames(Pointer))//'",', &
                  MAXVAL(MasterMaxHeap(:,Pointer)),sum(MasterMaxHeap(:,Pointer)), &
                  sum(MasterMaxHeap(:,Pointer))/ntasks
            end if        
         end do
         write (unit=trace_unit,fmt='("</table></body></html>")')

         if (trace_csv) then
            write(unit=trace_csv_unit,fmt=*)
         end if
      end if
   end if

   if (trace_write .AND. trace_unit /= 0) then
      close(trace_unit)
   end if
  
   if (trace_csv .and. rootproc()) then
      close(trace_csv_unit)
   end if

   if (mytask == trace_pe) then
      deallocate(index)
   end if

end subroutine trace_report


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


subroutine da_proc_sum_int (value)

   !---------------------------------------------------------------------------
   !  Purpose: Do MPI sum operation across processors to get the global sum of
   !           an integer value. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication, unlike wrf_dm_sum_int, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: value     ! Value on processor.

#ifdef DM_PARALLEL
   integer :: sum                   ! Sum across processors.

   ! Don't trace, as called within trace routines
   ! if (trace_use_frequent) call trace_entry("da_proc_sum_int")

   sum=0
   call mpi_reduce(value, sum, 1, mpi_integer, mpi_sum, root, &
      local_communicator, ierr)

   if (rootproc()) value = sum

   ! if (trace_use_frequent) call trace_exit("da_proc_sum_int")
#endif

end subroutine da_proc_sum_int


subroutine da_proc_sum_ints (values)

   !---------------------------------------------------------------------------
   !  Purpose: Do MPI sum operation across processors to get the global sum of
   !           an integer array. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication, unlike wrf_dm_sum_ints, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: values(:) ! Values

#ifdef DM_PARALLEL
   integer, allocatable :: sums(:) ! Sum across processors.


   ! Don't trace, as called within trace routines
   ! if (trace_use_frequent) call trace_entry("da_proc_sum_ints")

   allocate (sums(size(values)))
   sums(:)=0
   call mpi_reduce(values(:), sums(:), size(values), mpi_integer, mpi_sum, &
      root, local_communicator, ierr)

   if (rootproc()) values(:) = sums(:)
   deallocate(sums)

   ! if (trace_use_frequent) call trace_exit("da_proc_sum_ints")
#endif

end subroutine da_proc_sum_ints


subroutine da_proc_sum_real (value)

   !---------------------------------------------------------------------------
   ! Purpose: Do MPI reduction operation across processors to sum a real 
   ! vector.  
   ! On return, each of the N components of the vector "value" represents
   ! the sum of parts from each processor. The result is stored only on 
   ! the root processor, i.e., processor 0. (In this way, we do not have 
   ! to do all-to-all communication, unlike wrf_dm_sum_real, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   real, intent(inout) :: value(:) ! Array to be summed componentwise.

#ifdef DM_PARALLEL
   real              :: apsum(size(value))             ! Sum across processors.

   ! Don't trace, as called within trace routines
   !if (trace_use_frequent) call trace_entry("da_proc_sum_real")

   apsum(:)=0
   call mpi_reduce(value, apsum, SIZE(value), mpi_real8, mpi_sum, root, &
      local_communicator, ierr)

   if (rootproc()) value = apsum

   !if (trace_use_frequent) call trace_exit("da_proc_sum_real")
#endif

end subroutine da_proc_sum_real

end module module_trace

LOGICAL FUNCTION rootproc()
   USE module_dm
   IMPLICIT NONE
#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif
   INTEGER tsk, ierr, mpi_comm_local
#ifdef DM_PARALLEL
   CALL wrf_get_dm_communicator( mpi_comm_local )
   CALL mpi_comm_rank ( mpi_comm_local, tsk , ierr )
   rootproc = tsk .EQ. 0
#else
   tsk = 0
   rootproc = tsk .EQ. 0
#endif
   RETURN
END FUNCTION rootproc
