SUBROUTINE proc_sum_int ( value )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI sum operation across processors to get the global sum of
!           an integer value. The sum is returned only on the root processor,
!           i.e., processor 0. (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   INTEGER,   INTENT(INOUT)      :: value     ! Value on processor.

   INTEGER           :: sum                   ! Sum across processors.
   INTEGER           :: ierr                  ! Error code from MPI routines.
   INTEGER           :: myrank                ! Id of my processor.
   INTEGER           :: root                  ! Monitor processor.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr )

   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )
   sum=0
   call MPI_REDUCE(value, sum, 1, MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF ( myrank .eq. root ) value = sum
   RETURN
END SUBROUTINE proc_sum_int

