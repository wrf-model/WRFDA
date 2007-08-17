subroutine da_proc_sum_real (value)

   !---------------------------------------------------------------------------
   ! Purpose: Do MPI reduction operation across processors to sum a real 
   ! vector.  
   ! On return, each of the N components of the vector "value" represents
   ! the sum of parts from each processor. The result is stored only on 
   ! the root processor, i.e., processor 0. (In this way, we do not have 
   ! to do all-to-all communication, unlike wrf_dm_sum_real, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   real,    intent(inout)      :: value(:) ! Array to be summed componentwise.

#ifdef DM_PARALLEL
   real              :: apsum(size(value))             ! Sum across processors.

   apsum(:)=0
   call mpi_reduce(value, apsum, SIZE(value), true_mpi_real, mpi_sum, root, &
      comm, ierr)

   if (rootproc) value = apsum
#endif

end subroutine da_proc_sum_real


