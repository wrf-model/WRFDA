subroutine da_proc_maxmin_combine( n, max, min )

   !---------------------------------------------------------------------------
   !  PURPOSE: Do MPI reduction operations across processors to get the minimum
   !           and maximum values for an observation field of length n. The
   !           i, j location of the minimum and maximum, for each n, is also
   !           communicated.
   !           The return values are stored only on the root processor, i.e., 
   !           processor 0.  (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------
   
   use da_tools

   implicit none

   integer,   intent(in)                    :: n       ! Length of input fields.
   type (maxmin_field_type), intent(inout)  :: max(n)  ! Max values over proc.
   type (maxmin_field_type), intent(inout)  :: min(n)  ! Min values over proc.

   real    :: in(2,n)            ! MPI_REDUCE input value with processor rank.
   real    :: out(2,n)           ! MPI_REDUCE output min/max with associated processor.
   integer :: i                  ! Loop counter.
   integer :: proc_id(n)         ! Id of processor with max or min value.
   integer :: root               ! Monitor processor.
   integer :: ierr               ! Error code from MPI routines.
   integer :: status(mpi_status_size) ! MPI status.

#ifdef DM_PARALLEL

   if (rootproc) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )

   ! Get minimum value and associated processor index.
   do i = 1, n
      in(1,i) = min(i)%value
      in(2,i) = myproc
   end do

   call MPI_REDUCE(in, out, n, MPI_2doUBLE_PRECISION, MPI_MinLOC, &
                                     root, mpi_comm_world, IERR )

   if (rootproc) then
      do i = 1, n
         min(i)%value = out(1,i)
         proc_id(i) = inT(out(2,i))
      end do
   end if

   call wrf_dm_bcast_integer ( proc_id, n )

   ! Get i and j where minimum occurs.
   do i = 1, n
      if (proc_id(i) .ne. root) then
         if (rootproc) then
            call mpi_recv( min(i)%i, 1, mpi_integer, proc_id(i), &
                           10, mpi_comm_world, STATUS, IERR )
            call mpi_recv( min(i)%j, 1, mpi_integer, proc_id(i), &
                           11, mpi_comm_world, STATUS, IERR )
          else if (myproc .eq. proc_id(i)) then
             call mpi_send( min(i)%i, 1, mpi_integer, root, &
                             10, mpi_comm_world, IERR )
            call mpi_send( min(i)%j, 1, mpi_integer, root, &
                             11, mpi_comm_world, IERR )
         end if
      end if
   end do

   ! Get maximum value and associated processor index.
   do i = 1, n
      in(1,i) = max(i)%value
      in(2,i) = myproc
   end do
   call MPI_REDUCE(in, out, n, MPI_2doUBLE_PRECISION, MPI_MAXLOC, &
                                     root, mpi_comm_world, IERR )

   if (rootproc) then
      do i = 1, n
         max(i)%value = out(1,i)
         proc_id(i) = inT(out(2,i))
      end do
   end if

   call wrf_dm_bcast_integer ( proc_id, n )

   ! Get i and j where maximum occurs.
   do i = 1, n
      if (proc_id(i) .ne. root) then
         if (rootproc) then
            call mpi_recv( max(i)%i, 1, mpi_integer, proc_id(i), &
                           10, mpi_comm_world, STATUS, IERR )
            call mpi_recv( max(i)%j, 1, mpi_integer, proc_id(i), &
                           11, mpi_comm_world, STATUS, IERR )
         else if (myproc .eq. proc_id(i) ) then
            call mpi_send( max(i)%i, 1, mpi_integer, root, &
                             10, mpi_comm_world, IERR )
            call mpi_send( max(i)%j, 1, mpi_integer, root, &
                             11, mpi_comm_world, IERR )
         end if
      end if
   end do
#endif

end subroutine da_proc_maxmin_combine

