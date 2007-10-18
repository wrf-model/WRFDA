subroutine da_ao_stats_airsr (stats_unit, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Compute analysis increment at AIRS retrieval locations
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type (y_type),  intent(in)    :: re            ! A - O

   type (stats_airsr_type) :: stats
   integer                 :: nt, nq
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_ao_stats_airsr")

   nt = 0
   nq = 0

   stats%maximum%t = maxmin_type (missing_r, 0, 0)
   stats%maximum%q = maxmin_type (missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%minimum%q = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_airsr1_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%nlocal(airsr)
      if (iv%info(airsr)%proc_domain(1,n)) then
         do k=1, iv%info(airsr)%levels(n)
            call da_stats_calculate (n, k, iv%airsr(n)%t(k)%qc, & 
               re%airsr(n)%t(k), nt, &
               stats%minimum%t, stats%maximum%t, &
               stats%average%t, stats%rms_err%t)
            call da_stats_calculate (n, k, iv%airsr(n)%q(k)%qc, & 
               re%airsr(n)%q(k), nq, &
               stats%minimum%q, stats%maximum%q, &
               stats%average%q, stats%rms_err%q)
         end do
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nt)
   call da_proc_sum_int (nq)
   iv%nstats(airsr) = nt + nq

   call da_proc_stats_combine(stats%average%t, stats%rms_err%t, &
      stats%minimum%t%value, stats%maximum%t%value, &
      stats%minimum%t%n, stats%maximum%t%n, &
      stats%minimum%t%l, stats%maximum%t%l)
   call da_proc_stats_combine(stats%average%q, stats%rms_err%q, &
      stats%minimum%q%value, stats%maximum%q%value, &
      stats%minimum%q%n, stats%maximum%q%n, &
      stats%minimum%q%l, stats%maximum%q%l)

   if (rootproc) then
      if (nt /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for airs retrievals'
         call da_print_stats_airsr(stats_unit, nt, nq, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_airsr")

end subroutine da_ao_stats_airsr


, DYN_EM)

      call da_trace("da_transform_vtoy_adj","Starting da_run_wrfplus_ad.ksh")

#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then
         call da_system("da_run_wrfplus_ad.ksh pre")

         if (rootproc) then
            call da_system("rm -rf wrf_done")
            call da_system("touch wrf_go_ahead")
            call da_get_unit(wrf_done_unit)
            do while (.true.)
               open(wrf_done_unit,file="wrf_done",status="old",err=303)
               close(wrf_done_unit)
               exit
303            continue
               call da_system("sleep 1")
            end do
            call da_free_unit(wrf_done_unit)
         end if
         ! Wait until PE 0 agrees that AD finished
         call wrf_get_dm_communicator ( comm )
         call mpi_barrier(comm, ierr)
         call da_system("da_run_wrfplus_ad.ksh post")
      end if
#else
      call da_system("da_run_wrfplus_ad.ksh")
#endif
      call da_trace("da_transform_vtoy_adj","Finished da_run_wrfplus_ad.ksh")

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
   else  ! var4d
      iv%time = 1
      iv%info(:)%n1 = iv%plocal(iv%time-1,:) + 1
      iv%info(:)%n2 = iv%plocal(iv%time,:)
      call da_zero_x(grid%xa)
      call da_transform_xtoy_adj(grid, iv, y,grid%xa)
   end if ! var4d

   cv = 0.0
   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


