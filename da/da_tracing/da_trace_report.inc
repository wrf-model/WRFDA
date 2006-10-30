subroutine da_trace_report

   IMPLICIT NONE

   INTEGER :: Loop1                        ! loop counter
   INTEGER :: Loop2                        ! loop counter
   INTEGER :: i
   INTEGER :: j
   INTEGER :: CountRate
   INTEGER :: MasterNoRoutines
   INTEGER :: istat
   INTEGER :: temp
   INTEGER :: MinElapsedPos
   INTEGER :: MaxElapsedPos
   INTEGER :: MinCPUPos
   INTEGER :: MaxCPUPos
   INTEGER :: itemp1(MaxNoRoutines)
   INTEGER :: MasterMaxHeap(0:num_procs-1,MaxNoRoutines)
   INTEGER :: MasterNoCalls(0:num_procs-1,MaxNoRoutines)
   INTEGER :: OverallNoCalls(MaxNoRoutines)
   INTEGER, ALLOCATABLE :: Index(:)

   REAL    :: TempCpuTime
   REAL    :: TotalElapsedTime             !
   REAL    :: TotalCPUTime(1)              !
   REAL    :: SpeedUp                      ! speed up factor
   REAL    :: PercentCPUTime               ! percentage in CPU time
   REAL    :: PercentElapsedTime           ! percentage in elapsed time
   REAL    :: rtemp1(MaxNoRoutines)
   REAL    :: MasterElapsedTime(0:num_procs-1,MaxNoRoutines)
   REAL    :: MasterElapsedTimeLocal(0:num_procs-1,MaxNoRoutines)
   REAL    :: MasterCPUTime(0:num_procs-1,MaxNoRoutines)
   REAL    :: MasterCPUTimeLocal(0:num_procs-1,MaxNoRoutines)
   REAL    :: OverallElapsedTime(MaxNoRoutines)
   REAL    :: OverallCPUTime(MaxNoRoutines)

   CHARACTER (LEN=TraceNameLen) :: MasterTimerNames(MaxNoRoutines)

   integer :: iost

   !==========================================================================
   ! start of code
   !==========================================================================


   if (.not. use_html) then
      write (unit=trace_unit, FMT='(A)') &
         "Report only available if use_html is TRUE"
      return
   end if

   call SYSTEM_CLOCK (COUNT=temp)

   TotalElapsedTime=temp-BaseElapsedTime ! on PE 0

   call CPU_TIME(TempCpuTime)

   TotalCPUTime(1) = TempCpuTime - BaseCPUTime

   call SYSTEM_CLOCK( &
      COUNT_RATE=CountRate)

   ! Ensure the lists from each PE match. Use the routine list from the 
   ! traced PE as the master copy

   MasterTimerNames(:)=TimerNames(:)

   IF (myproc == trace_pe) THEN
      MasterNoRoutines=NoRoutines
   ELSE
      MasterNoRoutines=0
   END IF

#ifdef DM_PARALLEL
   call da_proc_sum_int(MasterNoRoutines)
   ! Only PE 0 knows the result

   call MPI_BCAST(MasterTimerNames(1:MaxNoRoutines), &
                  TraceNameLen*MaxNoRoutines, &
                  MPI_CHARACTER,trace_pe, MPI_COMM_WORLD,istat)

!   call GC_CBCAST(1001,TraceNameLen*MasterNoRoutines,0,num_procs,istat, &
!      MasterTimerNames(1:MasterNoRoutines))  
#endif

   MasterElapsedTime(:,:)=0.0
   MasterCPUTime(:,:)=0.0
   MasterElapsedTimeLocal(:,:)=0.0
   MasterCPUTimeLocal(:,:)=0.0
   MasterNoCalls(:,:)=0
   MasterMaxHeap(:,:)=0

   DO Loop1=1,MaxNoRoutines
      DO Loop2=1,NoRoutines
         IF (TimerNames(Loop2) == MasterTimerNames(Loop1)) THEN
            MasterElapsedTime(myproc,Loop1)=ElapsedTime(Loop2)
            MasterCPUTime(myproc,Loop1)=CPUTime(Loop2)
            MasterElapsedTimeLocal(myproc,Loop1)=ElapsedTimeLocal(Loop2)
            MasterCPUTimeLocal(myproc,Loop1)=CPUTimeLocal(Loop2)
            MasterNoCalls(myproc,Loop1)=NoCalls(Loop2)
            MasterMaxHeap(myproc,Loop1)=MaxHeap(Loop2)
            CYCLE
         END IF
      END DO
   END DO

#ifdef DM_PARALLEL
   DO i=0,num_procs-1
      call da_proc_sum_real(MaxNoRoutines,MasterElapsedTime(i,:))
      call da_proc_sum_real(MaxNoRoutines,MasterCPUTime(i,:))
      call da_proc_sum_real(MaxNoRoutines,MasterElapsedTimeLocal(i,:))
      call da_proc_sum_real(MaxNoRoutines,MasterCPUTimeLocal(i,:))
      call da_proc_sum_ints(MasterNoCalls(i,:))
      call da_proc_sum_ints(MasterMaxHeap(i,:))
   END DO
#endif

   IF (rootproc) THEN

      DO j=1,MasterNoRoutines
         rtemp1(j)=SUM(MasterElapsedTimeLocal(:,j))
      END DO
      !==========================================================================
      ! Sort subroutines into time order based on local Elapsed Time.
      ! All PEs should have the same sort order after the sum.
      !==========================================================================

      ALLOCATE (Index(MasterNoRoutines))

      call da_trace_real_sort(rtemp1,MasterNoRoutines,index)

      DO j=1,MasterNoRoutines
         OverallElapsedTime(j)=SUM(MasterElapsedTimeLocal(:,j))
         OverallCPUTime(j)=SUM(MasterCPUTimeLocal(:,j))
         OverallNoCalls(j)=SUM(MasterNoCalls(:,j))
      END DO

      WRITE(UNIT=trace_unit, FMT='(A,I4,A)') &
         "</pre><hr><H1>For PE",trace_pe,"</H1>"

      ! Output timing information

      WRITE(UNIT=trace_unit, &
         FMT='("<a name=local><h2>Local Timing Summary</h2></a>")')

      IF (num_procs == 1) THEN
         ! needs changing to work MPP
         WRITE (UNIT=trace_unit, &
            FMT='("(Tracing itself took ",F8.1,"s CPU Time, and ",F8.1,a)') &
            (CPUTimeLocalStart-CPUTimeStart(1)-TotalCPUTime(1)), &
            (ElapsedTimeLocalStart-ElapsedTimeStart(1)-TotalElapsedTime) / &
            REAL(CountRate), &
            "s elapsed time during the run. This is not included in the times below.)<p>"
      ELSE
         WRITE (UNIT=trace_unit,FMT='(A)') &
            "No estimate can be made of time in trace itself.<p>"
      END IF

      WRITE(UNIT=trace_unit, &
         FMT='("<TABLE BORDER>")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      WRITE(UNIT=trace_unit, &
         FMT='("CPU Time (seconds)<TH>Speed up")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum <TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TH>",I4," PE")') num_procs

      DO loop1=MasterNoRoutines,1,-1
         Pointer=index(loop1)    

         IF (TotalCPUTime(1) > 0.0) THEN
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         ELSE
           PercentCPUTime=100.0
         END IF

         IF (TotalElapsedTime > 0.0) THEN
            PercentElapsedTime=100.0*OverallElapsedTime(Pointer)/(REAL(num_procs) * TotalElapsedTime)
         ELSE
            PercentElapsedTime=100.0
         END IF

         IF (OverallElapsedTime(Pointer) > 0.0) THEN
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/REAL(CountRate*num_procs))
         ELSE
            SpeedUp = 0.0
         END IF

         ! This horrible solution as MINLOC does not take a DIM argument, so SUM
         ! is needed to convert the array to an integer

         MinElapsedPos = SUM(MINLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MaxElapsedPos = SUM(MAXLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MinCPUPos     = SUM(MINLOC(MasterCPUTimeLocal(:,Pointer)))-1
         MaxCPUPos     = SUM(MAXLOC(MasterCPUTimeLocal(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         WRITE (UNIT=trace_unit, FMT='(7A)') &
            "<TR><TD><a href=", &
            TRIM(Documentation_url), &
            "/", &
            TRIM(MasterTimerNames(Pointer)), & ! Subroutine name
            ".html>", &
            TRIM(MasterTimerNames(Pointer)), & ! Subroutine name
            "</a>"
         WRITE (UNIT=trace_unit, &
            FMT='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            REAL(OverallNoCalls(Pointer))/REAL(num_procs),                       & ! Average number of calls per PE
            OverallElapsedTime(Pointer)/(num_procs*REAL(CountRate)),             & ! Average Elapsed Time
            PercentElapsedTime,                                              & ! Percent Elapsed Time
            MasterElapsedTimeLocal(MinElapsedPos,Pointer)/REAL(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                                   & ! Which PE
            MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/REAL(CountRate),   & ! Max average Elapsed Time
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
               fmt='(A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"'//TRIM(MasterTimerNames(Pointer))//'",', &
               REAL(OverallNoCalls(Pointer))/REAL(num_procs),                       & ! Average number of calls per PE
               OverallElapsedTime(Pointer)/(num_procs*REAL(CountRate)),             & ! Average Elapsed Time
               PercentElapsedTime,                                              & ! Percent Elapsed Time
               MasterElapsedTimeLocal(MinElapsedPos,Pointer)/REAL(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                                   & ! Which PE
               MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/REAL(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                                   & ! Which PE
               OverallCPUTime(Pointer),                                         & ! CPU time
               PercentCPUTime,                                                  & ! Percent CPU time
               MasterCPUTimeLocal(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                       & ! Which PE
               MasterCPUTimeLocal(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                       & ! Which PE
               SpeedUp                                                            ! Speedup
         end if
      END DO

      WRITE (UNIT=trace_unit, &
         FMT='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "<TD></TD><TD><B>", &
         TotalElapsedTime/REAL(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>", &
         TotalCPUTime(1),"</B><TD></TD><TD></TD><TD></TD>"
      IF (TotalElapsedTime > 0.0) THEN
         WRITE (UNIT=trace_unit, FMT='("<TD><B>",F8.1,"</B>")') &
            (TotalCPUTime(1))/(TotalElapsedTime/REAL(CountRate))
      END IF
      WRITE(UNIT=trace_unit, &
         FMT='("</TABLE><p><p>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,FMT=*) " "
      end if

      !===================================================================================
      ! Sort subroutines into time order based on overall Elapsed Time.
      ! All PEs should have the same sort order after the sum. 
      !===================================================================================

      DO j=1,MasterNoRoutines
         rtemp1(j)=SUM(MasterElapsedTime(:,j))
      END DO

      call da_trace_real_sort(rtemp1,MasterNoRoutines,index)

      DO j=1,MasterNoRoutines
         OverallElapsedTime(j)=SUM(MasterElapsedTime(:,j))
         OverallCPUTime(j)=SUM(MasterCPUTime(:,j))
      END DO

      ! Output timing information

      WRITE(UNIT=trace_unit, &
         FMT='("</pre><hr><a name=overall><h2>Overall Timing Summary</h2></a>")')

      WRITE(UNIT=trace_unit, &
         FMT='("<TABLE BORDER>")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      WRITE(UNIT=trace_unit, &
         FMT='("CPU Time (seconds)<TH>Speed up")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum<TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      WRITE(UNIT=trace_unit, &
         FMT='("<TH>",I4," PE")') num_procs

      DO loop1=MasterNoRoutines,1,-1
         Pointer=index(loop1)    

         IF (TotalCPUTime(1) > 0.0) THEN
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         ELSE
            PercentCPUTime=100.0
         END IF

         IF (TotalElapsedTime > 0.0) THEN
            PercentElapsedTime=100.0 * OverallElapsedTime(Pointer)/(REAL(num_procs) * TotalElapsedTime)
         ELSE
            PercentElapsedTime=100.0
         END IF

         IF (OverallElapsedTime(Pointer) > 0.0) THEN
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/REAL(num_procs*CountRate))
         ELSE
            SpeedUp = 0.0
         END IF

         ! This horrible solution as MINLOC does not take a DIM argument, so 
         ! SUM is needed to convert the array to an integer

         MinElapsedPos = SUM(MINLOC(MasterElapsedTime(:,Pointer)))-1
         MaxElapsedPos = SUM(MAXLOC(MasterElapsedTime(:,Pointer)))-1
         MinCPUPos     = SUM(MINLOC(MasterCPUTime(:,Pointer)))-1
         MaxCPUPos     = SUM(MaxLOC(MasterCPUTime(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         WRITE (UNIT=trace_unit, FMT='(7A)') &
            "<TR><TD><a href=", &
            TRIM(Documentation_url), &
            "/", &
            TRIM(MasterTimerNames(Pointer)), &    ! Subroutine name
            ".html>", &
            TRIM(MasterTimerNames(Pointer)), &    ! Subroutine name
            "</a>"
         WRITE (UNIT=trace_unit, &
            FMT='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            REAL(OverallNoCalls(Pointer))/REAL(num_procs),                  & ! Number of calls per PE
            OverallElapsedTime(Pointer)/(REAL(num_procs*CountRate)),        & ! Average Elapsed Time
            PercentElapsedTime,                                         & ! Percent Elapsed Time
            MasterElapsedTime(MinElapsedPos,Pointer)/REAL(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                              & ! Which PE
            MasterElapsedTime(MaxElapsedPos,Pointer)/REAL(CountRate),   & ! Max average Elapsed Time
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
               fmt='(A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"'//TRIM(MasterTimerNames(Pointer))//'",', &
               REAL(OverallNoCalls(Pointer))/REAL(num_procs),                  & ! Number of calls per PE
               OverallElapsedTime(Pointer)/(REAL(num_procs*CountRate)),        & ! Average Elapsed Time
               PercentElapsedTime,                                         & ! Percent Elapsed Time
               MasterElapsedTime(MinElapsedPos,Pointer)/REAL(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                              & ! Which PE
               MasterElapsedTime(MaxElapsedPos,Pointer)/REAL(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                              & ! Which PE
               OverallCPUTime(Pointer),                                    & ! CPU time
               PercentCPUTime,                                             & ! Percent CPU time
               MasterCPUTime(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                  & ! Which PE
               MasterCPUTime(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                  & ! Which PE
               SpeedUp                                                       ! SpeedUp
         end if
      END DO

      WRITE (UNIT=trace_unit, &
         FMT='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "</TD><TD><TD><B>", &
         TotalElapsedTime/REAL(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>",TotalCPUTime(1), &
         "</B><TD></TD><TD></TD><TD></TD>"
      IF (TotalElapsedTime > 0.0) THEN
         WRITE (UNIT=trace_unit, FMT='("<TD><B>",F8.1,"</B>")') &
            TotalCPUTime(1)/(TotalElapsedTime/REAL(CountRate))
      END IF

      WRITE(UNIT=trace_unit, &
         FMT='("</TABLE>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,FMT=*) " "
      end if

   END IF ! rootproc

   !============================================================================
   ! Sort subroutines into memory use order by max memory on one PE
   !============================================================================

   IF (trace_memory) THEN

#ifdef DM_PARALLEL
      DO i=0,num_procs-1
         DO j=1,MaxNoRoutines
            call da_proc_sum_int(MasterMaxHeap(i,j))
         END DO
      END DO
#endif

      IF (rootproc) THEN
         DO j=1,MasterNoRoutines
            itemp1(j)=MAXVAL(MasterMaxHeap(:,j))
         END DO

         call da_trace_int_sort(itemp1,MasterNoRoutines,index)

         WRITE (UNIT=trace_unit,FMT='("<hr><a name=memory><h2>Maximum memory usage for routines</h2></a>")')
         WRITE (UNIT=trace_unit,FMT='("<TABLE BORDER>")')
         WRITE (UNIT=trace_unit,FMT='("<TR><TH>Routine<TH>Max in any PE (bytes)")')
         WRITE (UNIT=trace_unit,FMT='("<TH>Overall (bytes)<TH>Average per PE (bytes)")')

         DO loop1=MasterNoRoutines,1,-1
            Pointer=index(loop1)
            WRITE (UNIT=trace_unit, &
               FMT='("<TR><TD><a href=",A,"/",A,".html>",A,"</a><TD>",I11,"<TD>",I11,"<TD>",I11)') &
               TRIM(Documentation_url),TRIM(TimerNames(Pointer)),TRIM(TimerNames(Pointer)), &
               MAXVAL(MasterMaxHeap(:,Pointer)),SUM(MasterMaxHeap(:,Pointer)), &
               SUM(MasterMaxHeap(:,Pointer))/num_procs
            if (trace_csv) then
               WRITE (UNIT=trace_csv_unit, &
                  FMT='(A,I11,",",I11,",",I11)') &
                  '"'//TRIM(TimerNames(Pointer))//'",', &
                  MAXVAL(MasterMaxHeap(:,Pointer)),SUM(MasterMaxHeap(:,Pointer)), &
                  SUM(MasterMaxHeap(:,Pointer))/num_procs
            end if        
         END DO
         WRITE (UNIT=trace_unit,FMT='("</table></body></html>")')

         if (trace_csv) then
            write(unit=trace_csv_unit,FMT=*)
         end if
      END IF
   END IF

   IF (trace_write .AND. trace_unit /= 0) THEN
      CLOSE(trace_unit)
   END IF
  
   if (trace_csv .and. rootproc) then
      close(trace_csv_unit)
   end if

   IF (myproc == 0) THEN
      DEALLOCATE(index)
   END IF

end subroutine da_trace_report
