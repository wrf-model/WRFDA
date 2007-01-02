SUBROUTINE proc_diagnostics_collect(num_sound, iv, re )

!------------------------------------------------------------------------------
!  PURPOSE: Do MPI reduction operations across processors to get values needed
!           for diagnostics output. These are stored only on the root processor, 
!           i.e., processor 0. (In this way, we do not have to do all-to-all 
!           communication.)
!
!------------------------------------------------------------------------------

   INCLUDE 'mpif.h'

   integer, intent(in)                       :: num_sound
   type (sound_type), intent(inout)          :: iv(:)
   type (residual_sound_type), intent(inout) :: re(:)

   INTEGER          :: sum_levels         ! Total number of levels (all obs)
   INTEGER          :: nlevels            ! Number of levels for nth obs.
   INTEGER          :: obs_level          ! Index into levels for nth obs.
   INTEGER          :: n                  ! Loop counter over observations.
   INTEGER          :: k                  ! Loop counter over levels.
   LOGICAL, POINTER :: on_proc(:)
   REAL, POINTER    :: u(:)
   REAL, POINTER    :: v(:)
   REAL, POINTER    :: t(:)
   REAL, POINTER    :: q(:)

   INTEGER :: myrank             ! Id of my processor.
   INTEGER :: nprocs             ! Number of processors.
   INTEGER :: iproc              ! Loop counter.
   INTEGER :: root               ! Monitor processor.
   INTEGER :: ierr               ! Error code from MPI routines.
   INTEGER :: status(mpi_status_size) ! MPI status.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

! Allocate buffer.
   sum_levels = 0
   ! Calculate size of buffers.
   do n = 1, num_sound
     nlevels = iv (n) % sfc % info % levels
     sum_levels = sum_levels + nlevels
   end do

   ALLOCATE ( on_proc(1:num_sound) )
   ALLOCATE ( u(1:sum_levels) )
   ALLOCATE ( v(1:sum_levels) )
   ALLOCATE ( t(1:sum_levels) )
   ALLOCATE ( q(1:sum_levels) )

   u(1:sum_levels) = 0.0
   v(1:sum_levels) = 0.0
   t(1:sum_levels) = 0.0
   q(1:sum_levels) = 0.0

! Get rank of monitor processor and broadcast to others.
   call MPI_COMM_RANK( MPI_COMM_WORLD, myrank, ierr ) 
   call MPI_COMM_SIZE( MPI_COMM_WORLD, nprocs, ierr )
   IF ( wrf_dm_on_monitor() ) root = myrank
   call wrf_dm_bcast_integer ( root, 1 )

! Message pass on_proc and iv field arrays.
   if( myrank .ne. root ) then
!    Assign on_proc and field arrays.
     obs_level = 0
     do n = 1, num_sound
       on_proc(n) = iv (n) % loc % proc_domain
       nlevels    = iv (n) % info % levels

       if( on_proc(n) ) then
         do k = 1, nlevels
           u(obs_level+k) =  iv(n) % u(k) % inv
           v(obs_level+k) =  iv(n) % v(k) % inv
           t(obs_level+k) =  iv(n) % t(k) % inv
           q(obs_level+k) =  iv(n) % q(k) % inv
         end do
       end if
       obs_level = obs_level + nlevels
     end do
     call MPI_SEND( on_proc, num_sound, MPI_LOGICAL, root, &
                    30, MPI_COMM_WORLD, IERR )
     call MPI_SEND( u, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    31, MPI_COMM_WORLD, IERR )
     call MPI_SEND( v, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    32, MPI_COMM_WORLD, IERR )
     call MPI_SEND( t, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    33, MPI_COMM_WORLD, IERR )
     call MPI_SEND( q, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    34, MPI_COMM_WORLD, IERR )

   else
      
     do iproc = 1, nprocs-1

       call MPI_RECV(on_proc, num_sound, MPI_LOGICAL, MPI_ANY_SOURCE, &
                     30, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(u, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     31, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(v, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     32, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(t, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     33, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(q, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     34, MPI_COMM_WORLD, STATUS, IERR )

       obs_level = 0
       do n = 1, num_sound
         nlevels    = iv (n) % info % levels
         if( on_proc(n) ) then
           do k = 1, nlevels
             iv(n) % u(k) % inv = u(obs_level+k)
             iv(n) % v(k) % inv = v(obs_level+k)
             iv(n) % t(k) % inv = t(obs_level+k)
             iv(n) % q(k) % inv = q(obs_level+k)
           end do
         end if
         obs_level = obs_level + nlevels

       end do
     end do

   end if

! Message pass re field arrays.
   if( myrank .ne. root ) then
!    Assign field arrays.
     obs_level = 0
     do n = 1, num_sound
       nlevels    = iv (n) % info % levels

       if( on_proc(n) ) then
         do k = 1, nlevels
           u(obs_level+k) =  re(n) % u(k)
           v(obs_level+k) =  re(n) % v(k)
           t(obs_level+k) =  re(n) % t(k)
           q(obs_level+k) =  re(n) % q(k)
         end do
       end if
       obs_level = obs_level + nlevels
     end do
     call MPI_SEND( u, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    31, MPI_COMM_WORLD, IERR )
     call MPI_SEND( v, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    32, MPI_COMM_WORLD, IERR )
     call MPI_SEND( t, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    33, MPI_COMM_WORLD, IERR )
     call MPI_SEND( q, sum_levels, MPI_DOUBLE_PRECISION, root, &
                    34, MPI_COMM_WORLD, IERR )

   else
      
     do iproc = 1, nprocs-1

       call MPI_RECV(u, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     31, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(v, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     32, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(t, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     33, MPI_COMM_WORLD, STATUS, IERR )
       call MPI_RECV(q, sum_levels, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, &
                     34, MPI_COMM_WORLD, STATUS, IERR )

       obs_level = 0
       do n = 1, num_sound
         nlevels    = iv (n) % info % levels
         if( on_proc(n) ) then
           do k = 1, nlevels
             re(n) % u(k) = u(obs_level+k)
             re(n) % v(k) = v(obs_level+k)
             re(n) % t(k) = t(obs_level+k)
             re(n) % q(k) = q(obs_level+k)
           end do
         end if
         obs_level = obs_level + nlevels

       end do
     end do

   end if

   DEALLOCATE ( on_proc )
   DEALLOCATE ( u )
   DEALLOCATE ( v )
   DEALLOCATE ( t )
   DEALLOCATE ( q )
   RETURN
END SUBROUTINE proc_diagnostics_collect

