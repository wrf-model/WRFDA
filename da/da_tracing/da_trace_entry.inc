! Trace entry point to subroutine

subroutine da_trace_entry( &
   Name, &                       ! in
   Message, &                    ! in, optional
   Messages, &                   ! in, optional
   MaxNoCalls)                   ! in, optional

   IMPLICIT NONE

   ! Arguments (order does not obey coding standard, special case)

   CHARACTER (LEN=*),           INTENT(IN) :: Name         ! Routine name
   CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Message      ! message
   CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Messages(:)  ! message array
   INTEGER, OPTIONAL,           INTENT(IN) :: MaxNoCalls   ! max no calls to show

   ! Local variables

   INTEGER           :: IOStatus        ! I-O return code
   INTEGER           :: Loop            ! General loop counter
   INTEGER           :: Count
   INTEGER           :: OldPointer
   INTEGER           :: TotalSpace
   INTEGER           :: LocalMaxNoCalls
   REAL              :: CPUTime1
   REAL              :: temp1
   REAL              :: temp2
   LOGICAL           :: NewRoutine

   !-----------------------------------------------------------------------
   ! Start of code
   !-----------------------------------------------------------------------

   ! Stop the clock

   call CPU_TIME(CPUTime1)

   call SYSTEM_CLOCK( &
      COUNT=Count)

   !-----------------------------------------------------------------------
   ! check if tracing active. If not check whether to switch it on
   !-----------------------------------------------------------------------

   IF (.NOT. TraceActive) THEN
      IF (trace_start_points == 0) THEN
         ! start with first call
         TraceActive = .TRUE.
      ELSE
         DO Loop=1,trace_start_points
            IF (Name == TraceNames(Loop)(1:LEN(Name))) THEN
               TraceActive    = .TRUE.
               TraceDepth     = 0
               TraceStartedBy = Name
               EXIT
            END IF
         END DO
      END IF
      IF (.NOT. TraceActive) THEN
         ! did not want to start trace, so leave
         RETURN
      END IF
   END IF

   !-----------------------------------------------------------------------
   ! timing and maximum heap usage
   !-----------------------------------------------------------------------

   ! Increment the local elapsed time and local CPU time since the
   ! last trace entry, if any

   IF (Pointer /= 0) THEN
      temp1 = REAL(Count - BaseElapsedTime) - ElapsedTimeLocalStart
      temp2 = CPUTime1 - CPUTimeLocalStart
      ElapsedTimeLocal(Pointer)    = ElapsedTimeLocal(Pointer) + temp1
      ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
      CPUTimeLocal(Pointer)        = CPUTimeLocal(Pointer) + temp2
      CPUTimeThisCall(Pointer)     = CPUTimeThisCall(Pointer) + temp2
   END IF

   OldPointer=Pointer

   ! Check subroutine name 

   NewRoutine = .TRUE.
   DO Pointer=1,NoRoutines     
      IF (TimerNames(Pointer) == Name) THEN
         NewRoutine=.FALSE.
         EXIT
      END IF
   END DO

   IF (NewRoutine) THEN
      ! New subroutine entered
      IF( NoRoutines >= MaxNoRoutines )THEN ! too many to trace
          call da_error(__FILE__,__LINE__, &
             (/"Too many routines. Not timing " // Name/))

         !All the timings etc are put instead to the calling routine,
         ! which therefore may have incorrect summaries.
         !The best solution is to increase MaxNoRoutines.
         Pointer = OldPointer
         ! Fix to get the correct NoCalls(OldPointer) despite the +1 later
         NoCalls(Pointer)=NoCalls(Pointer)-1

      ELSE ! Pointer=NoRoutines+1 (from the end of earlier DO loop)
         NoRoutines=NoRoutines+1
         TimerNames(NoRoutines)=Name
      END IF
   END IF

   NoCalls(Pointer)=NoCalls(Pointer)+1

   CPUTimeThisCall(Pointer)     = 0.0
   ElapsedTimeThisCall(Pointer) = 0.0

   CalledBy(Pointer)=OldPointer

   IF (trace_memory) THEN
      call da_memory( &
         TotalSpace)
      EntryHeap(Pointer) = TotalSpace
      LastSpace = TotalSpace
      IF (MaxHeap(Pointer) < TotalSpace) THEN
         MaxHeap(Pointer) = TotalSpace
      END IF
   ELSE
      TotalSpace = 0
   END IF

   IF (trace_write .AND. TraceDepth <= trace_max_depth ) THEN

      IF (PRESENT(MaxNoCalls)) THEN
         LocalMaxNoCalls = MaxNoCalls
      ELSE
         LocalMaxNoCalls = trace_repeat_head
      END IF

      IF (NoCalls(Pointer) <= LocalMaxNoCalls) THEN
         IF (trace_memory) THEN
            if (use_html) then
               WRITE (UNIT=trace_unit, &
                  FMT='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>",I11,I11)', &
                  IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Documentation_url), &
                  TRIM(Name),TRIM(Name), TotalSpace,Count
            else
               WRITE (UNIT=trace_unit, &
                  FMT='(A,"> ",A,I11)', &
                  IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Name), TotalSpace
           end if
         ELSE
            if (use_html) then
               WRITE (UNIT=trace_unit, &
                  FMT='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>")', &
                  IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Documentation_url), &
                  TRIM(Name),TRIM(Name)
            else
               WRITE (UNIT=trace_unit, FMT='(A,"> ",A)', IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Name)
            end if
         END IF
         IF (IOStatus /= 0) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Cannot write to trace file for "//TRIM(Name)/))
         END IF

         IF (PRESENT(Message)) THEN
            WRITE (UNIT=trace_unit, FMT='(A," ",A)', IOSTAT=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),TRIM(Message)
            IF (IOStatus .NE. 0) THEN
               call da_error(__FILE__,__LINE__, &
                  (/"Cannot write to trace file for "//TRIM(Name)/))
            END IF
         END IF

         IF (PRESENT(Messages)) THEN
            DO Loop = 1, SIZE(Messages)
               WRITE (UNIT=trace_unit, FMT='(A," ",A)', IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Messages(Loop))
               IF (IOStatus .NE. 0) THEN
                  call da_error(__FILE__,__LINE__, &
                     (/"Cannot write to trace file for "//TRIM(Name)/))
               END IF
            END DO ! Loop
         END IF
      END IF

   END IF ! trace_write

   TraceDepth=TraceDepth+1

   call SYSTEM_CLOCK( &
      COUNT=Count)

   call CPU_TIME(CPUTime1)

   ! set the start elapsed and CPU time both locally and generally

   ElapsedTimeStart(Pointer) = REAL(Count-BaseElapsedTime)
   ElapsedTimeLocalStart     = REAL(Count-BaseElapsedTime)

   CPUTimeStart(Pointer) = CPUTime1
   CPUTimeLocalStart     = CPUTime1

   ! call FLUSH(trace_unit)

   RETURN
end subroutine da_trace_entry


