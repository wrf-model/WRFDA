subroutine da_trace_init

   IMPLICIT NONE

   ! Local variables

   INTEGER :: IOStatus             ! I/O return code
   INTEGER :: Loop1
   CHARACTER (LEN=200) :: TraceFile

   !===========================================================================

   ! Start of code

   IOStatus = 0

#ifdef DM_PARALLEL
   call wrf_get_nproc(nproc)
   call wrf_get_myproc(mype)
#else
   nproc=1
   mype=0
#endif

   IF (trace_all_pes .OR. mype == trace_pe) THEN
     trace_write = .TRUE.  
   END IF

   !-----------------------------------------------------------------
   ! Open trace output file. 
   !-----------------------------------------------------------------

   IF (trace_write .AND. trace_unit /= 0) THEN
      WRITE(UNIT=TraceFile,FMT='(A,I0,A)') &
        "trace/",mype,".html"
      OPEN ( &
         UNIT=trace_unit, &   ! I:
         FILE=TRIM(TraceFile), &   ! I:
         STATUS="REPLACE", & ! I:
         ACTION="WRITE", &   ! I:
         IOSTAT=IOStatus)    ! O:
   
      IF (IOStatus /= 0) THEN
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open trace file "//TraceFile/))
      END IF
   END IF

   !-----------------------------------------------------------------
   ! Find out whether to trace memory usage. The Cray routine to check
   ! memory usage is very slow, so it is best to only switch on memory
   ! checking if actively required.
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! Set up timing and memory usage
   !-----------------------------------------------------------------

   DO Loop1=1,MaxNoRoutines
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
   END DO

   Pointer     = 0
   NoRoutines  = 0

   call SYSTEM_CLOCK( &
      COUNT=BaseElapsedTime)

   call CPU_TIME(BaseCPUTime)

   ! start trace output here so memory calculations are not distorted
   ! by IO buffer being grabbed later

   TraceDepth = 0

   IF (trace_write) THEN
      WRITE (UNIT=trace_unit,FMT='(A)') "<html><head><title>Tracing</title></head>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<body><h1>Trace Output</h1>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<ul>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<li><a href=#tree>Calling Tree</a>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<li><a href=#local>Local routine timings</a>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<li><a href=#overall>Overall routine timings</a>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<li><a href=#memory>Memory usage</a>"
      WRITE (UNIT=trace_unit,FMT='(A)') "</ul>"
      WRITE (UNIT=trace_unit,FMT='(A)') "Links are to documentation set"
      WRITE (UNIT=trace_unit,FMT='(A)') "<a href=",TRIM(Documentation_url), &
         ">",TRIM(Documentation_url),"</a>"
      WRITE (UNIT=trace_unit,FMT='(A)') ", <strong>NOT</strong> to exact code under test.<p>"
      WRITE (UNIT=trace_unit,FMT='(A)') "<a name=tree><h2>Calling Tree</h2></a><pre>"
   END IF

   RETURN

end subroutine da_trace_init
