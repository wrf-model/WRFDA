subroutine da_y_facade_to_global( re_slice, template, re_glob_slice )

   !---------------------------------------------------------------------------
   ! PURPOSE:  Collect a local y_facade_type object into a global y_facade_type 
   !           object while preserving serial-code storage order.  This is 
   !           used to perform bitwise-exact floating-point summations in 
   !           serial-code-order during bitwise-exact testing of 
   !           distributed-memory parallel configurations.  
   !
   ! METHOD:   Indices stowed away during Read_Obs() are used to restore serial 
   !           storage order.  Memory for global objects is allocated here.  
   !           Global objects are minimally allocated to save memory.  
   !           Memory must be deallocated by the caller via a call to 
   !           da_y_facade_free().  
   !           Memory for local object re_slice is deallocated here.  Do not 
   !           use re_slice after this call.  
   !           The "template" argument is needed because some tasks may not 
   !           have any local obs.  
   !
   ! PERFORMANCE NOTE:   This implementation is NOT very efficient.  Speed it 
   !                     up if testing is too slow.  
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   ! task-local objects  (really INTENT(IN   ))
   TYPE (y_facade_type), INTENT(INOUT) :: re_slice      ! residual vector
   TYPE (residual_template_type), INTENT(IN) :: template  ! allocation info
   ! task-global objects (really INTENT(  OUT))
   TYPE (y_facade_type), INTENT(INOUT) :: re_glob_slice ! residual vector

   ! Local declarations
#ifdef DM_PARALLEL
   INTEGER                      :: n, k, serial_n, serial_numvals
   INTEGER                      :: communicator
   INTEGER                      :: proc
   INTEGER                      :: num_obs_send
   INTEGER                      :: buf_idx
   INTEGER                      :: num_obs_all
   INTEGER                      :: num_recv_all
   INTEGER                      :: obs_global_index(re_slice%num_obs)
   INTEGER                      :: num_values(re_slice%num_obs)
   INTEGER                      :: num_send_buf_lcl
   INTEGER,ALLOCATABLE          :: num_obs_send_all(:)
   INTEGER,ALLOCATABLE          :: obs_global_index_all(:)
   INTEGER,ALLOCATABLE          :: obs_global_index_inv(:)
   INTEGER,ALLOCATABLE          :: obs_start_all(:)  ! start index of each obs
   INTEGER,POINTER              :: num_values_all(:)
   INTEGER,ALLOCATABLE          :: num_send_buf_all(:)
   INTEGER,ALLOCATABLE          :: recv_counts(:)
   INTEGER,ALLOCATABLE          :: recv_displs(:)
   REAL,ALLOCATABLE             :: re_vals_lcl(:)
   REAL,ALLOCATABLE             :: re_vals_all(:)
   INTEGER                      :: ierr
   INTEGER                      :: i

   ! TODO:  later, upgrade from "allgather" to "gather-broadcast"

   ! collect information about all observations
   num_obs_send = 0
   obs_global_index = -1
   DO n=1, re_slice%num_obs
      IF ( re_slice%obs(n)%proc_domain ) THEN
         num_obs_send = num_obs_send + 1
         obs_global_index(num_obs_send) = re_slice%obs(n)%obs_global_index
      ENDIF
   ENDDO
   DO n=1, num_obs_send
      IF ( obs_global_index(n) < 0 ) THEN
         call da_error(__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  bad local value of obs_global_index'/))
      ENDIF
   END DO
   ! exchange num_obs_send and obs_global_index
   call wrf_get_dm_communicator ( communicator )
   ALLOCATE( num_obs_send_all(0:num_procs-1),     &
              num_send_buf_all(0:num_procs-1),     &
              recv_counts(0:num_procs-1),          &
              recv_displs(0:num_procs-1) )
   ! gather num_obs_send
   call MPI_ALLGATHER( num_obs_send, 1, MPI_INTEGER, num_obs_send_all, 1, &
                        MPI_INTEGER, communicator, ierr )
   num_obs_all = SUM( num_obs_send_all )
   IF ( num_obs_all /= re_slice%num_obs_glo ) THEN
      call da_error (__FILE__,__LINE__, &
         (/'ASSERTION ERROR:  inconsistent count of sound obs'/))
   ENDIF
   ! set up to gather obs_global_index
   recv_counts = num_obs_send_all
   recv_displs(0) = 0
   DO proc=1, num_procs-1
      recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
   ENDDO
   ALLOCATE( num_values_all(num_obs_all),       &
              obs_global_index_all(num_obs_all), &
              obs_global_index_inv(num_obs_all), &
              obs_start_all(num_obs_all) )
   ! gather obs_global_index
   call MPI_ALLGATHERV( obs_global_index, num_obs_send, MPI_INTEGER,    &
                         obs_global_index_all, recv_counts, recv_displs, &
                         MPI_INTEGER, communicator, ierr )
   ! invert obs_global_index_all
   ! initialize to "invalid" value
   obs_global_index_inv = -1
   DO n=1, num_obs_all
      IF ( ( obs_global_index_all(n) <  1 ) .OR. &
           ( obs_global_index_all(n) > SIZE(obs_global_index_inv) ) ) THEN
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs_global_index_all(n) out of range'/))
      ENDIF
      IF ( obs_global_index_inv(obs_global_index_all(n)) /= -1 ) THEN
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs owned by more than one task'/))
      ENDIF
      obs_global_index_inv(obs_global_index_all(n)) = n
   ENDDO
   DO n=1, num_obs_all
      IF ( obs_global_index_inv(obs_global_index_all(n)) == -1 ) THEN
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs not owned by any task'/))
      ENDIF
   ENDDO

   ! Create re_glob_slice and populate with residual_generic_type objects 
   ! allocated to match template.  
   call da_y_facade_create( re_glob_slice, num_obs_all, num_obs_all, &
                          template=template )

   ! NOTE:  This i loop should be inside the n loops.  
   ! Ideally, residual_generic class should know how to pack/unpack 
   ! (serialize/deserialize) itself for arbitrary I/O or communications (MPI or 
   ! otherwise) that clients may choose to implement.  Below are a possible set 
   ! of new methods for residual_generic_type:  
   !  residual_generic_pack_counts( res_generic, (OUT)rcount, (OUT)icount )
   !  [client allocates ibuf(icount) and rbuf(rcount)]
   !  residual_generic_pack( res_generic, (INOUT)rstart, (INOUT)rbuf, &
   !                         (INOUT)istart, (INOUT)ibuf )
   !  [client MPI communications:   ibuf->ibufg  rbuf->rbufg]
   !  residual_generic_unpack_counts( ibufg, (OUT)rstarts, (OUT)rcounts )
   !  residual_generic_unpack( (OUT)res_generic, rstarts(n), rcounts(n), rbufg )
   ! TODO:  
   !  1) Design serialization for residual_generic_type.  
   !  2) Implement new methods.  
   !  3) Refactor below...  
   !  4) Optimize performance.  
   ! At the moment this refactoring is relatively low-priority since 
   ! da_y_facade_to_global() is already well-encapsulated and peformance is not 
   ! (yet) a concern for testing.  
   ! Loop over vector and scalar elements
   
   DO i=template%lbnd,template%ubnd
      num_obs_send = 0
      num_values = 0
      num_send_buf_lcl = 0
      ! collect information to allocate buffers
      DO n=1, re_slice%num_obs
         IF ( re_slice%obs(n)%proc_domain ) THEN
            num_obs_send = num_obs_send + 1
            ! track number of scalars/levels per obs element
            num_values(num_obs_send) = SIZE(re_slice%obs(n)%values(i)%ptr)
            ! track total buffer size
            num_send_buf_lcl = num_send_buf_lcl + num_values(num_obs_send)
         ENDIF
      ENDDO
      ! gather num_send_buf_lcl
      call MPI_ALLGATHER( num_send_buf_lcl, 1, MPI_INTEGER, &
                          num_send_buf_all, 1,              &
                          MPI_INTEGER, communicator, ierr )
      ! gather num_values
      recv_counts = num_obs_send_all
      recv_displs(0) = 0
      DO proc=1, num_procs-1
         recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
      ENDDO
      ! num_values
      call MPI_ALLGATHERV( num_values, num_obs_send, MPI_INTEGER,    &
                           num_values_all, recv_counts, recv_displs, &
                           MPI_INTEGER, communicator, ierr )
      ! set up to gather local arrays
      ! compute start index of each obs in gathered buffers
      obs_start_all(1) = 1
      DO n=2, num_obs_all
         obs_start_all(n) = obs_start_all(n-1) + num_values_all(n-1)
      ENDDO
      ! finish setting up to gather local arrays
      recv_counts = num_send_buf_all
      recv_displs(0) = 0
      DO proc=1, num_procs-1
         recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
      ENDDO
      num_recv_all = SUM( recv_counts )
      ! allocate and initialize send buffer
      ALLOCATE( re_vals_lcl(num_send_buf_lcl) )
      buf_idx = 0
      DO n=1, re_slice%num_obs
         IF ( re_slice%obs(n)%proc_domain ) THEN
            DO k=1, SIZE(re_slice%obs(n)%values(i)%ptr)
               buf_idx = buf_idx + 1
               re_vals_lcl(buf_idx) = re_slice%obs(n)%values(i)%ptr(k)
            ENDDO
         ENDIF
      END DO
      IF ( buf_idx /= num_send_buf_lcl ) THEN
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  buf_idx /= num_send_buf_lcl'/))
      ENDIF
      ! allocate receive buffer
      ALLOCATE( re_vals_all(num_recv_all) )
      ! finally, actually gather the values
      call MPI_ALLGATHERV( re_vals_lcl, num_send_buf_lcl, TRUE_MPI_REAL,   &
                           re_vals_all, recv_counts, recv_displs, &
                           TRUE_MPI_REAL, communicator, ierr )
      ! initialize re_glob_slice
      ! NOTE:  The i loop should be inside this n loop, see comment above...  
      ! TODO:  Refactor...  
      DO n=1, re_glob_slice%num_obs
         serial_n = obs_global_index_inv(n)
         serial_numvals = num_values_all(serial_n)
         buf_idx = obs_start_all(serial_n)
         ! note that we only collected obs someone owns
         re_glob_slice%obs(n)%proc_domain = .TRUE.
         re_glob_slice%obs(n)%obs_global_index = obs_global_index_all(serial_n)
         call da_res_generic_alloc_and_set( re_glob_slice%obs(n), i, &
            re_vals_all(buf_idx:(buf_idx+serial_numvals-1)) )
      END DO
      ! deallocate communication buffers, etc.
      DEALLOCATE( re_vals_all )
      DEALLOCATE( re_vals_lcl ) 

   ENDDO  ! end of i loop

   call da_y_facade_free( re_slice )

   DEALLOCATE( num_values_all, obs_global_index_all, &
                obs_global_index_inv, obs_start_all )
   DEALLOCATE( num_obs_send_all, num_send_buf_all, &
                recv_counts, recv_displs )

#else
   call da_error (__FILE__,__LINE__, &
      (/'may only be called for a distributed memory parallel run'/))
#endif

end subroutine da_y_facade_to_global

