SUBROUTINE proc_stats_combine( proc_ave, proc_err, proc_min, proc_max, &
                               nobs_min, nobs_max, klev_min, klev_max )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get the average, 
!           rms error, minimum, and maximum values for an observation field.
!           These are stored only on the root processor, i.e., processor 0.
!           (In this way, we do not have to do all-to-all communication.)
!
!------------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   REAL,      INTENT(INOUT)      :: proc_ave       ! Processor average.
   REAL,      INTENT(INOUT)      :: proc_err       ! Processor rms error.
   REAL,      INTENT(INOUT)      :: proc_min       ! Processor minumum.
   REAL,      INTENT(INOUT)      :: proc_max       ! Processor maximum.
   INTEGER,   INTENT(INOUT)      :: nobs_min       ! Obs number of minimum.
   INTEGER,   INTENT(INOUT)      :: nobs_max       ! Obs number of maximum.
   INTEGER,   INTENT(INOUT)      :: klev_min       ! Level of minimum.
   INTEGER,   INTENT(INOUT)      :: klev_max       ! Level of maximum.

   REAL    :: average            ! Global average.
   REAL    :: rms_err            ! Global rms_error.
   REAL    :: in(2)              ! MPI_REDUCE input value with processor rank.
   REAL    :: out(2)             ! MPI_REDUCE output min/max with associated processor.
   INTEGER :: proc_id            ! Id of processor with max or min value.
   INTEGER :: myrank             ! Id of my processor.
   INTEGER :: root               ! Monitor processor.
   INTEGER :: ierr               ! Error code from MPI routines.
   INTEGER :: status(mpi_status_size) ! MPI status.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr ) 
   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )

! Sum average and rms error over all processors and store on monitor processor.
   call MPI_REDUCE(proc_ave, average, 1, TRUE_MPI_REAL, MPI_SUM, root, MPI_COMM_WORLD, IERR )
   call MPI_REDUCE(proc_err, rms_err, 1, TRUE_MPI_REAL, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF ( wrf_dm_on_monitor() ) THEN
     proc_ave = average
     proc_err = rms_err
   END IF

! Get minimum value and associated processor index.
   in(1) = proc_min
   in(2) = myrank
   call MPI_REDUCE(in, out, 1, MPI_2DOUBLE_PRECISION, MPI_MINLOC, root, MPI_COMM_WORLD, IERR )

   if( myrank .eq. root ) then
     proc_min = out(1)
     proc_id = out(2)
   end if
   call wrf_dm_bcast_integer ( proc_id, 1 )

! Get obs number and k-level where minimum occurs.
   if( proc_id .ne. root ) then
     if( myrank .eq. root ) then
       call MPI_RECV( nobs_min, 1, MPI_INTEGER, proc_id, 10, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV( klev_min, 1, MPI_INTEGER, proc_id, 11, MPI_COMM_WORLD, STATUS, IERR )
     else if( myrank .eq. proc_id ) then
       call MPI_SEND( nobs_min, 1, MPI_INTEGER, root, 10, MPI_COMM_WORLD, IERR )
       call MPI_SEND( klev_min, 1, MPI_INTEGER, root, 11, MPI_COMM_WORLD, IERR )
     end if
   end if

! Get maximum value and associated processor index.
   in(1) = proc_max
   in(2) = myrank

   call MPI_REDUCE(in, out, 1, MPI_2DOUBLE_PRECISION, MPI_MAXLOC, root, MPI_COMM_WORLD, IERR )

   if( myrank .eq. root ) then
     proc_max = out(1)
     proc_id = out(2)
   end if

   call wrf_dm_bcast_integer ( proc_id, 1 )

! Get obs number and k-level where maximum occurs.
   if( proc_id .ne. root ) then
     if( myrank .eq. root ) then
       call MPI_RECV( nobs_max, 1, MPI_INTEGER, proc_id, 10, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV( klev_max, 1, MPI_INTEGER, proc_id, 11, MPI_COMM_WORLD, STATUS, IERR )
     else if( myrank .eq. proc_id ) then
       call MPI_SEND( nobs_max, 1, MPI_INTEGER, root, 10, MPI_COMM_WORLD, IERR )
       call MPI_SEND( klev_max, 1, MPI_INTEGER, root, 11, MPI_COMM_WORLD, IERR )
     end if
   end if
   RETURN
END SUBROUTINE proc_stats_combine

