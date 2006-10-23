subroutine da_proc_sum_int ( value )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI sum operation across processors to get the global sum of
   !           an integer value. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------

   implicit none

   INTEGER,   INTENT(INOUT)      :: value     ! Value on processor.

   INTEGER           :: sum                   ! Sum across processors.
   INTEGER           :: ierr                  ! Error code from MPI routines.
   INTEGER           :: root                  ! Monitor processor.

#ifdef DM_PARALLEL
   IF (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   sum=0
   call MPI_REDUCE(value, sum, 1, MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF (myproc .eq. root) value = sum
#endif

end subroutine da_proc_sum_int

