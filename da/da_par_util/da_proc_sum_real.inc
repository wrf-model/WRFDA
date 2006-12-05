subroutine da_proc_sum_real ( n, value )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operation across processors to sum a real vector.  
   !           On return, each of the N components of the vector "value" represents
   !           the sum of parts from each processor. The result is stored only on 
   !           the root processor, i.e., processor 0. (In this way, we do not have 
   !           to do all-to-all communication.)
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: n        ! Length of value array. 
   real,    intent(inout)      :: value(n) ! Array to be summed componentwise.

   real              :: apsum(n)             ! Sum across processors.
   integer           :: root               ! Monitor processor.

#ifdef DM_PARALLEL

   if (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )
   apsum=0
   call mpi_reduce(value, apsum, n, true_mpi_real, mpi_sum, root, comm, ierr )

   if (myproc .eq. root) value = apsum
#endif

end subroutine da_proc_sum_real

