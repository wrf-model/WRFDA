! General trace within a subroutine

subroutine da_trace( &
   Name,     &           ! in
   Message,  &           ! in, optional
   Messages,  &          ! in, optional
   MaxNoCalls)           ! in, optional

   IMPLICIT NONE

   ! Arguments

   CHARACTER (LEN=*), INTENT(IN)           :: Name         ! Subroutine name
   CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Message      ! Text to trace
   CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Messages(:)  ! Text to trace
   INTEGER, OPTIONAL, INTENT(IN)           :: MaxNoCalls   ! max no calls to show

   ! Local variables

   INTEGER           :: IOStatus     ! I-O return code
   INTEGER           :: Loop         ! General loop counter
   INTEGER           :: TotalSpace
   INTEGER           :: LocalMaxNoCalls
   CHARACTER(LEN=25) :: Change

   !-----------------------------------------------------------------------
   ! Start of code
   !-----------------------------------------------------------------------

   !-----------------------------------------------------------------------
   ! Check whether trace active and depth of trace
   !-----------------------------------------------------------------------

   IF (.NOT. TraceActive) THEN
      RETURN
   END IF

   IF (TraceDepth >= trace_max_depth) THEN
      ! already at maximum depth, so return
      RETURN
   END IF

   !-----------------------------------------------------------------------
   ! Note memory usage
   !-----------------------------------------------------------------------

   Change = ""

   IF (trace_memory) THEN
      call da_memory( &
         TotalSpace)
      IF (LastSpace < TotalSpace) THEN
         WRITE(Change,"(A9,I12)")", bigger", TotalSpace - LastSpace
      ELSE IF (LastSpace > TotalSpace) THEN
         WRITE(Change,"(A9,I12)")", smaller", TotalSpace - LastSpace
      END IF
      IF (MaxHeap(Pointer) < TotalSpace) THEN
         MaxHeap(Pointer) = TotalSpace
      END IF
      LastSpace = TotalSpace
   ELSE
      TotalSpace = 0
   END IF

   !-----------------------------------------------------------------------
   ! Perform the trace if not done too many times before. Only on PE 0
   !-----------------------------------------------------------------------  

   IF (trace_write) THEN

      IF (PRESENT(MaxNoCalls)) THEN
         LocalMaxNoCalls = MaxNoCalls
      ELSE
         LocalMaxNoCalls = trace_repeat_body
      END IF

      NoCallsBody(Pointer) = NoCallsBody(Pointer)+1

      IF (NoCallsBody(Pointer) <= LocalMaxNoCalls) THEN
         IF (trace_memory) THEN
             if (use_html) then
                WRITE (UNIT=trace_unit, &
                   FMT='(A, "| <a href=",A,"/",A,".html>",A,"</a>",I11,A)', &
                   IOSTAT=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),TRIM(Documentation_url), &
                   TRIM(Name), TRIM(Name), TotalSpace, Change
             else
                WRITE (UNIT=trace_unit, &
                   FMT='(A, "| ",A,I11,A)', &
                   IOSTAT=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),TRIM(Name), TotalSpace, Change
            end if
         ELSE
            if (use_html) then
               WRITE (UNIT=trace_unit, &
                  FMT='(A, "| <a href=",A,"/",A,".html>",A,"</a>")', &
                  IOSTAT=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),TRIM(Documentation_url), &
                  TRIM(Name), TRIM(Name)  
            else 
               WRITE (UNIT=trace_unit, &
                  FMT='(A, "| ",A)', &
                  IOSTAT=IOStatus) &
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

      IF (NoCallsBody(Pointer) == trace_repeat_body) THEN
         WRITE (UNIT=trace_unit, FMT='(A,"  Called enough, going quiet")', IOSTAT=IOStatus) &
            pad(1:TraceDepth*TraceIndentAmount)
         IF (IOStatus .NE. 0) THEN
            call da_error(__FILE__,__LINE__, &
              (/"Cannot write to trace file for "//TRIM(Name)/))
         END IF
      END IF
   END IF ! trace_write

end subroutine da_trace


