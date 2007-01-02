SUBROUTINE proc_maxmin_combine( n, max, min )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get the minimum
!           and maximum values for an observation field of length n. The
!           i, j location of the minimum and maximum, for each n, is also
!           communicated.
!           The return values are stored only on the root processor, i.e., 
!           processor 0.  (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   INTEGER,   INTENT(IN)                    :: n       ! Length of input fields.
   TYPE (maxmin_field_type), INTENT(INOUT)  :: max(n)  ! Max values over proc.
   TYPE (maxmin_field_type), INTENT(INOUT)  :: min(n)  ! Min values over proc.

   REAL    :: in(2,n)            ! MPI_REDUCE input value with processor rank.
   REAL    :: out(2,n)           ! MPI_REDUCE output min/max with associated processor.
   INTEGER :: i                  ! Loop counter.
   INTEGER :: proc_id(n)         ! Id of processor with max or min value.
   INTEGER :: myrank             ! Id of my processor.
   INTEGER :: root               ! Monitor processor.
   INTEGER :: ierr               ! Error code from MPI routines.
   INTEGER :: status(mpi_status_size) ! MPI status.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr ) 
   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )

! Get minimum value and associated processor index.
   do i = 1, n
     in(1,i) = min(i)%value
     in(2,i) = myrank
   end do

   call MPI_REDUCE(in, out, n, MPI_2DOUBLE_PRECISION, MPI_MINLOC, &
                                     root, MPI_COMM_WORLD, IERR )

   if( myrank .eq. root ) then
     do i = 1, n
       min(i)%value = out(1,i)
       proc_id(i) = out(2,i)
     end do
   end if

   call wrf_dm_bcast_integer ( proc_id, n )

! Get i and j where minimum occurs.
   do i = 1, n
     if( proc_id(i) .ne. root ) then
       if( myrank .eq. root ) then
         call MPI_RECV( min(i)%i, 1, MPI_INTEGER, proc_id(i), &
                           10, MPI_COMM_WORLD, STATUS, IERR )
         call MPI_RECV( min(i)%j, 1, MPI_INTEGER, proc_id(i), &
                           11, MPI_COMM_WORLD, STATUS, IERR )
       else if( myrank .eq. proc_id(i) ) then
         call MPI_SEND( min(i)%i, 1, MPI_INTEGER, root, &
                             10, MPI_COMM_WORLD, IERR )
         call MPI_SEND( min(i)%j, 1, MPI_INTEGER, root, &
                             11, MPI_COMM_WORLD, IERR )
       end if
     end if
   end do

! Get maximum value and associated processor index.
   do i = 1, n
     in(1,i) = max(i)%value
     in(2,i) = myrank
   end do
   call MPI_REDUCE(in, out, n, MPI_2DOUBLE_PRECISION, MPI_MAXLOC, &
                                     root, MPI_COMM_WORLD, IERR )

   if( myrank .eq. root ) then
     do i = 1, n
       max(i)%value = out(1,i)
       proc_id(i) = out(2,i)
     end do
   end if

   call wrf_dm_bcast_integer ( proc_id, n )

! Get i and j where maximum occurs.
   do i = 1, n
     if( proc_id(i) .ne. root ) then
       if( myrank .eq. root ) then
         call MPI_RECV( max(i)%i, 1, MPI_INTEGER, proc_id(i), &
                           10, MPI_COMM_WORLD, STATUS, IERR )
         call MPI_RECV( max(i)%j, 1, MPI_INTEGER, proc_id(i), &
                           11, MPI_COMM_WORLD, STATUS, IERR )
       else if( myrank .eq. proc_id(i) ) then
         call MPI_SEND( max(i)%i, 1, MPI_INTEGER, root, &
                             10, MPI_COMM_WORLD, IERR )
         call MPI_SEND( max(i)%j, 1, MPI_INTEGER, root, &
                             11, MPI_COMM_WORLD, IERR )
       end if
     end if
   end do

   RETURN
END SUBROUTINE proc_maxmin_combine

