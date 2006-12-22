subroutine da_proc_sum_int ( value )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI sum operation across processors to get the global sum of
   !           an integer value. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------

   implicit none

   integer,   intent(inout)      :: value     ! Value on processor.

   integer           :: sum                   ! Sum across processors.
   integer           :: root                  ! Monitor processor.

#ifdef DM_PARALLEL
   if (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   sum=0
   call mpi_reduce(value, sum, 1, mpi_integer, mpi_sum, root, comm, ierr )

   if (myproc .eq. root) value = sum
#endif

end subroutine da_proc_sum_int


