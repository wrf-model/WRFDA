subroutine da_proc_sum_ints ( values )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI sum operation across processors to get the global sum of
   !           an integer array. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------

   implicit none

   INTEGER,   INTENT(INOUT)      :: values(:) ! Values

   integer, allocatable :: sums(:) ! Sum across processors.
   INTEGER           :: ierr                  ! Error code from MPI routines.
   INTEGER           :: root                  ! Monitor processor.

#ifdef DM_PARALLEL
   IF (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   allocate (sums(size(values)))
   sums(:)=0
   call MPI_REDUCE(values(:), sums(:), size(values), MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF (rootproc) values(:) = sums(:)
   deallocate(sums)
#endif

end subroutine da_proc_sum_ints

