subroutine proc_sum_ints ( values )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI sum operation across processors to get the global sum of
   !           an integer array. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   implicit none

   integer,   intent(inout)      :: values(:) ! Values

   integer, allocatable :: sums(:) ! Sum across processors.
   integer           :: ierr                  ! Error code from MPI routines.
   integer           :: root                  ! Monitor processor.
   INTEGER           :: myrank                ! Id of my processor.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

#ifdef DM_PARALLEL

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr )

   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )
   allocate (sums(size(values)))
   sums(:)=0
   call MPI_REDUCE(values(:), sums(:), size(values), MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF ( myrank .eq. root ) values(:) = sums(:)
   deallocate(sums)
#endif

end subroutine proc_sum_ints

