subroutine da_proc_sum_real ( n, value )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operation across processors to sum a real vector.  
   !           On return, each of the N components of the vector "value" represents
   !           the sum of parts from each processor. The result is stored only on 
   !           the root processor, i.e., processor 0. (In this way, we do not have 
   !           to do all-to-all communication.)
   !---------------------------------------------------------------------------

   implicit none

   INTEGER, INTENT(IN)         :: n        ! Length of value array. 
   REAL,    INTENT(INOUT)      :: value(n) ! Array to be summed componentwise.

   REAL              :: apsum(n)             ! Sum across processors.
   INTEGER           :: ierr               ! Error code from MPI routines.
   INTEGER           :: root               ! Monitor processor.

#ifdef DM_PARALLEL

   IF (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   apsum=0
   call MPI_REDUCE(value, apsum, n, TRUE_MPI_REAL, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF (myproc .eq. root) value = apsum
#endif

end subroutine da_proc_sum_real

