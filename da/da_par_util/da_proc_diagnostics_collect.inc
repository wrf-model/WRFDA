subroutine da_proc_diagnostics_collect(num_sound, iv, re )

   !---------------------------------------------------------------------------
   ! PURPOSE: Do MPI reduction operations across processors to get values needed
   !          for diagnostics output. These are stored only on the root processor, 
   !          i.e., processor 0. (In this way, we do not have to do all-to-all 
   !          communication.)
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                       :: num_sound
   type (sound_type), intent(inout)          :: iv(:)
   type (residual_sound_type), intent(inout) :: re(:)

   integer          :: sum_levels         ! Total number of levels (all obs)
   integer          :: nlevels            ! Number of levels for nth obs.
   integer          :: obs_level          ! Index into levels for nth obs.
   integer          :: n                  ! Loop counter over observations.
   integer          :: k                  ! Loop counter over levels.
   logical, pointer :: on_proc(:)
   real, pointer    :: u(:)
   real, pointer    :: v(:)
   real, pointer    :: t(:)
   real, pointer    :: q(:)

   integer :: iproc              ! Loop counter.
   integer :: root               ! Monitor processor.
   integer :: status(mpi_status_size) ! MPI status.

#ifdef DM_PARALLEL

   ! Allocate buffer.
   sum_levels = 0
   ! Calculate size of buffers.
   do n = 1, num_sound
      nlevels = iv (n) % sfc % info % levels
      sum_levels = sum_levels + nlevels
   end do

   allocate ( on_proc(1:num_sound) )
   allocate ( u(1:sum_levels) )
   allocate ( v(1:sum_levels) )
   allocate ( t(1:sum_levels) )
   allocate ( q(1:sum_levels) )

   u(1:sum_levels) = 0.0
   v(1:sum_levels) = 0.0
   t(1:sum_levels) = 0.0
   q(1:sum_levels) = 0.0

   ! Get rank of monitor processor and broadcast to others.
   if ( rootproc ) root = myproc
   call wrf_dm_bcast_integer ( root, 1 )

   ! Message pass on_proc and iv field arrays.
   if (myproc .ne. root) then
      !    Assign on_proc and field arrays.
      obs_level = 0
      do n = 1, num_sound
         on_proc(n) = iv (n) % loc % proc_domain
         nlevels    = iv (n) % info % levels

         if (on_proc(n)) then
            do k = 1, nlevels
               u(obs_level+k) =  iv(n) % u(k) % inv
               v(obs_level+k) =  iv(n) % v(k) % inv
               t(obs_level+k) =  iv(n) % t(k) % inv
               q(obs_level+k) =  iv(n) % q(k) % inv
            end do
         end if
         obs_level = obs_level + nlevels
      end do
      call mpi_send( on_proc, num_sound, mpi_logical, root, &
                    30, comm, ierr )
      call mpi_send( u, sum_levels, mpi_double_precision, root, &
                    31, comm, ierr )
      call mpi_send( v, sum_levels, mpi_double_precision, root, &
                    32, comm, ierr )
      call mpi_send( t, sum_levels, mpi_double_precision, root, &
                    33, comm, ierr )
      call mpi_send( q, sum_levels, mpi_double_precision, root, &
                    34, comm, ierr )
   else    
      do iproc = 1, num_procs-1
         call mpi_recv(on_proc, num_sound, mpi_logical, mpi_any_source, &
                     30, comm, STATUS, ierr )
         call mpi_recv(u, sum_levels, mpi_double_precision, mpi_any_source, &
                     31, comm, STATUS, ierr )
         call mpi_recv(v, sum_levels, mpi_double_precision, mpi_any_source, &
                     32, comm, STATUS, ierr )
         call mpi_recv(t, sum_levels, mpi_double_precision, mpi_any_source, &
                     33, comm, STATUS, ierr )
         call mpi_recv(q, sum_levels, mpi_double_precision, mpi_any_source, &
                     34, comm, STATUS, ierr )

         obs_level = 0
         do n = 1, num_sound
            nlevels    = iv (n) % info % levels
            if (on_proc(n)) then
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
   if (myproc .ne. root) then
      ! Assign field arrays.
      obs_level = 0
      do n = 1, num_sound
         nlevels    = iv (n) % info % levels
         if (on_proc(n)) then
            do k = 1, nlevels
               u(obs_level+k) =  re(n) % u(k)
               v(obs_level+k) =  re(n) % v(k)
               t(obs_level+k) =  re(n) % t(k)
               q(obs_level+k) =  re(n) % q(k)
            end do
         end if
         obs_level = obs_level + nlevels
      end do
      call mpi_send( u, sum_levels, mpi_double_precision, root, &
                    31, comm, ierr )
      call mpi_send( v, sum_levels, mpi_double_precision, root, &
                    32, comm, ierr )
      call mpi_send( t, sum_levels, mpi_double_precision, root, &
                    33, comm, ierr )
      call mpi_send( q, sum_levels, mpi_double_precision, root, &
                    34, comm, ierr )
   else
      do iproc = 1, num_procs-1

         call mpi_recv(u, sum_levels, mpi_double_precision, mpi_any_source, &
                     31, comm, STATUS, ierr )
         call mpi_recv(v, sum_levels, mpi_double_precision, mpi_any_source, &
                     32, comm, STATUS, ierr )
         call mpi_recv(t, sum_levels, mpi_double_precision, mpi_any_source, &
                     33, comm, STATUS, ierr )
         call mpi_recv(q, sum_levels, mpi_double_precision, mpi_any_source, &
                     34, comm, STATUS, ierr )

         obs_level = 0
         do n = 1, num_sound
            nlevels    = iv (n) % info % levels
            if (on_proc(n)) then
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

   deallocate ( on_proc )
   deallocate ( u )
   deallocate ( v )
   deallocate ( t )
   deallocate ( q )
#endif

end subroutine da_proc_diagnostics_collect

