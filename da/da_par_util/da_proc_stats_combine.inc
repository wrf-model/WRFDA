subroutine da_proc_stats_combine( proc_ave, proc_err, proc_min, proc_max, &
                               nobs_min, nobs_max, klev_min, klev_max )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operations across processors to get the average, 
   !           rms error, minimum, and maximum values for an observation field.
   !           These are stored only on the root processor, i.e., processor 0.
   !           (In this way, we do not have to do all-to-all communication.)
   !---------------------------------------------------------------------------

   implicit none

   real,      intent(inout)      :: proc_ave       ! Processor average.
   real,      intent(inout)      :: proc_err       ! Processor rms error.
   real,      intent(inout)      :: proc_min       ! Processor minumum.
   real,      intent(inout)      :: proc_max       ! Processor maximum.
   integer,   intent(inout)      :: nobs_min       ! Obs number of minimum.
   integer,   intent(inout)      :: nobs_max       ! Obs number of maximum.
   integer,   intent(inout)      :: klev_min       ! Level of minimum.
   integer,   intent(inout)      :: klev_max       ! Level of maximum.

   real    :: average            ! Global average.
   real    :: rms_err            ! Global rms_error.
   real    :: in(2)              ! MPI_REDUCE input value with processor rank.
   real    :: out(2)             ! MPI_REDUCE output min/max with associated processor.
   integer :: proc_id            ! Id of processor with max or min value.
   integer :: root               ! Monitor processor.
   integer :: ierr               ! Error code from MPI routines.
   integer :: status(mpi_status_size) ! MPI status.

#ifdef DM_PARALLEL

   if (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )

   ! Sum average and rms error over all processors and store on monitor processor.
   call MPI_REDUCE(proc_ave, average, 1, true_mpi_real, mpi_sum, root, mpi_comm_world, IERR )
   call MPI_REDUCE(proc_err, rms_err, 1, true_mpi_real, mpi_sum, root, mpi_comm_world, IERR )

   if ( rootproc ) then
      proc_ave = average
      proc_err = rms_err
   end if

   ! Get minimum value and associated processor index.
   in(1) = proc_min
   in(2) = myproc
   call MPI_REDUCE(in, out, 1, MPI_2doUBLE_PRECISION, MPI_MinLOC, root, mpi_comm_world, IERR )

   if (myproc .eq. root) then
      proc_min = out(1)
      proc_id = inT(out(2))
   end if
   call wrf_dm_bcast_integer ( proc_id, 1 )

   ! Get obs number and k-level where minimum occurs.
   if (proc_id .ne. root) then
      if (myproc .eq. root) then
         call mpi_recv( nobs_min, 1, mpi_integer, proc_id, 10, mpi_comm_world, STATUS, IERR )
         call mpi_recv( klev_min, 1, mpi_integer, proc_id, 11, mpi_comm_world, STATUS, IERR )
      else if (myproc .eq. proc_id) then
         call mpi_send( nobs_min, 1, mpi_integer, root, 10, mpi_comm_world, IERR )
         call mpi_send( klev_min, 1, mpi_integer, root, 11, mpi_comm_world, IERR )
      end if
   end if

   ! Get maximum value and associated processor index.
   in(1) = proc_max
   in(2) = myproc

   call MPI_REDUCE(in, out, 1, MPI_2doUBLE_PRECISION, MPI_MAXLOC, root, mpi_comm_world, IERR )

   if (myproc .eq. root) then
      proc_max = out(1)
      proc_id = inT(out(2))
   end if

   call wrf_dm_bcast_integer ( proc_id, 1 )

   ! Get obs number and k-level where maximum occurs.
   if (proc_id .ne. root) then
      if (myproc .eq. root) then
         call mpi_recv( nobs_max, 1, mpi_integer, proc_id, 10, mpi_comm_world, STATUS, IERR )
         call mpi_recv( klev_max, 1, mpi_integer, proc_id, 11, mpi_comm_world, STATUS, IERR )
      else if( myproc .eq. proc_id ) then
         call mpi_send( nobs_max, 1, mpi_integer, root, 10, mpi_comm_world, IERR )
         call mpi_send( klev_max, 1, mpi_integer, root, 11, mpi_comm_world, IERR )
      end if
   end if
#endif

end subroutine da_proc_stats_combine

