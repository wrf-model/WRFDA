subroutine da_print_stats_airep(stats_unit, nu, nv, nt, airep)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt
   type (stats_airep_type), intent(in)    :: airep

   if (trace_use_dull) call da_trace_entry("da_print_stats_airep")

   write(unit=stats_unit, fmt='(4a/)') &
      '   var             ', &
      'u (m/s)     n    k    ', &
      'v (m/s)     n    k    ', &
      't (K)       n    k    '

   write(unit=stats_unit, fmt='(a,i16,3i22)') &
      '  Number: ', nu, nv, nt

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   if (nt < 1) nt = 1

   write(unit=stats_unit, fmt='((a,3(f12.4,2i5)))') &
      ' Minimum(n,k): ', airep%minimum%u, airep%minimum%v, airep%minimum%t, &
      ' Maximum(n,k): ', airep%maximum%u, airep%maximum%v, airep%maximum%t
   write(unit=stats_unit, fmt='((a,3(f12.4,10x)))') &
      ' Average     : ', airep%average%u/real(nu), airep%average%v/real(nv), airep%average%t/real(nt), &
      '    RMSE     : ', sqrt(airep%rms_err%u/real(nu)), sqrt(airep%rms_err%v/real(nv)), &
      sqrt(airep%rms_err%t/real(nt))

   if (trace_use_dull) call da_trace_exit("da_print_stats_airep")

end subroutine da_print_stats_airep


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


