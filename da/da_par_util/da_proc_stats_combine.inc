subroutine da_atotime(date_char, st)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=80), intent(in) :: date_char
   real,              intent(out) :: st
  
   integer                :: ccyy,mo,dd,hh,mi,ss,i
   integer, dimension(12) :: mmday
 
   mmday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  
   read(date_char(1:19),'(i4,1x,4(i2,1x),i2)') &
        ccyy, mo, dd, hh, mi, ss
  
   if (mod(ccyy,4) == 0) then
      mmday(2) = 29
      if (mod(ccyy,400) == 0) then
         mmday(2) = 29
      else if (mod(ccyy,100) == 0) then
         mmday(2) = 28
      end if
   end if
  
   dd=dd+365*(ccyy-2000)

   do i=1,mo-1
      dd=dd+mmday(i)
   end do
  
   st = real(ss) &
      + 60.0*(real(mi) &
      + 60.0*(real(hh) &
      + 24.0* real(dd)))

end subroutine da_atotime


  :: nobs_min       ! Obs number of minimum.
   integer,   intent(inout)      :: nobs_max       ! Obs number of maximum.
   integer,   intent(inout)      :: klev_min       ! Level of minimum.
   integer,   intent(inout)      :: klev_max       ! Level of maximum.

   real    :: average            ! Global average.
   real    :: rms_err            ! Global rms_error.
   real    :: in(2)              ! mpi_reduce input value with processor rank.
   real    :: out(2)             ! mpi_reduce output min/max with associated processor.
   integer :: proc_id            ! Id of processor with max or min value.
   integer :: status(mpi_status_size) ! MPI status.
   real    :: buf(1)

#ifdef DM_PARALLEL

   if (trace_use_frequent) call da_trace_entry("da_proc_stats_combine")

   ! Sum average and rms error over all processors and store on monitor processor.
   call mpi_reduce(proc_ave, average, 1, true_mpi_real, mpi_sum, root, comm, ierr)
   call mpi_reduce(proc_err, rms_err, 1, true_mpi_real, mpi_sum, root, comm, ierr)

   if (rootproc) then
      proc_ave = average
      proc_err = rms_err
   end if

   ! Get minimum value and associated processor index.
   in(1) = proc_min
   in(2) = myproc
   call mpi_reduce(in, out, 1, mpi_2double_precision, mpi_minloc, root, comm, ierr)

   if (myproc == root) then
      proc_min = out(1)
      proc_id = inT(out(2))
   end if

   buf(1) = real(proc_id)
   call wrf_dm_bcast_real (buf, 1)
   proc_id=int(buf(1))

   ! Get obs number and k-level where minimum occurs.
   if (proc_id .ne. root) then
      if (rootproc) then
         call mpi_recv(nobs_min, 1, mpi_integer, proc_id, 10, comm, STATUS, ierr)
         call mpi_recv(klev_min, 1, mpi_integer, proc_id, 11, comm, STATUS, ierr)
      else if (myproc == proc_id) then
         call mpi_send(nobs_min, 1, mpi_integer, root, 10, comm, ierr)
         call mpi_send(klev_min, 1, mpi_integer, root, 11, comm, ierr)
      end if
   end if

   ! Get maximum value and associated processor index.
   in(1) = proc_max
   in(2) = myproc

   call mpi_reduce(in, out, 1, mpi_2double_precision, mpi_maxloc, root, comm, ierr)

   if (rootproc) then
      proc_max = out(1)
      proc_id = int(out(2))
   end if

   buf(1) = real(proc_id)
   call wrf_dm_bcast_real (buf, 1)
   proc_id=int(buf(1))

   ! Get obs number and k-level where maximum occurs.
   if (proc_id .ne. root) then
      if (rootproc) then
         call mpi_recv(nobs_max, 1, mpi_integer, proc_id, 10, comm, STATUS, ierr)
         call mpi_recv(klev_max, 1, mpi_integer, proc_id, 11, comm, STATUS, ierr)
      else if (myproc == proc_id) then
         call mpi_send(nobs_max, 1, mpi_integer, root, 10, comm, ierr)
         call mpi_send(klev_max, 1, mpi_integer, root, 11, comm, ierr)
      end if
   end if

   if (trace_use_frequent) call da_trace_exit("da_proc_stats_combine")
#endif

end subroutine da_proc_stats_combine


