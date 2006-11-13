subroutine da_proc_sum_ints ( values )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI sum operation across processors to get the global sum of
   !           an integer array. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------

   implicit none

   integer,   intent(inout)      :: values(:) ! Values

   integer, allocatable :: sums(:) ! Sum across processors.
   integer           :: ierr                  ! Error code from MPI routines.
   integer           :: root                  ! Monitor processor.

#ifdef DM_PARALLEL
   if (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   allocate (sums(size(values)))
   sums(:)=0
   call MPI_REDUCE(values(:), sums(:), size(values), mpi_integer, mpi_sum, root, mpi_comm_world, IERR )

   if (rootproc) values(:) = sums(:)
   deallocate(sums)
#endif

end subroutine da_proc_sum_ints

