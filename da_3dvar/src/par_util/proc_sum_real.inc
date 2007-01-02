SUBROUTINE proc_sum_real ( n, value )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operation across processors to sum a real vector.  
!           On return, each of the N components of the vector "value" represents
!           the sum of parts from each processor. The result is stored only on 
!           the root processor, i.e., processor 0. (In this way, we do not have 
!           to do all-to-all communication.)
!
!------------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   INTEGER, INTENT(IN)         :: n        ! Length of value array. 
   REAL,    INTENT(INOUT)      :: value(n) ! Array to be summed componentwise.

   REAL              :: apsum(n)             ! Sum across processors.
   INTEGER           :: ierr               ! Error code from MPI routines.
   INTEGER           :: myrank             ! Id of my processor.
   INTEGER           :: root               ! Monitor processor.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr )

   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )
   apsum=0
   call MPI_REDUCE(value, apsum, n, TRUE_MPI_REAL, MPI_SUM, root, MPI_COMM_WORLD, IERR )

   IF ( myrank .eq. root ) value = apsum

   RETURN
END SUBROUTINE proc_sum_real

