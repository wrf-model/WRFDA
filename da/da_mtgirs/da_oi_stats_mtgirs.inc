subroutine da_oi_stats_mtgirs (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in)      :: iv            ! OI

   type (stats_mtgirs_type)          :: stats
   integer                          :: nu, nv, nt, nq
   integer                          :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_mtgirs")

   nu = 0
   nv = 0
   nt = 0
   nq = 0

   stats%maximum%u = maxmin_type(missing_r, 0, 0)
   stats%maximum%v = maxmin_type(missing_r, 0, 0)
   stats%maximum%t = maxmin_type(missing_r, 0, 0)
   stats%maximum%q = maxmin_type(missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%minimum%q = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_mtgirs1_type(0.0, 0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(mtgirs)%nlocal
      do k=1, iv%info(mtgirs)%levels(n)
!         if (iv%info(mtgirs)%proc_domain(k,n)) then
         if (iv%info(mtgirs)%proc_domain(1,n)) then
            call da_stats_calculate(iv%info(mtgirs)%obs_global_index(n), &
               k, iv%mtgirs(n)%u(k)%qc, iv%mtgirs(n)%u(k)%inv, nu, &
               stats%minimum%u, stats%maximum%u, stats%average%u, stats%rms_err%u)
            call da_stats_calculate(iv%info(mtgirs)%obs_global_index(n), &
               k, iv%mtgirs(n)%v(k)%qc, iv%mtgirs(n)%v(k)%inv, nv, &
               stats%minimum%v, stats%maximum%v, stats%average%v, stats%rms_err%v)
            call da_stats_calculate(iv%info(mtgirs)%obs_global_index(n), &
               k, iv%mtgirs(n)%t(k)%qc, iv%mtgirs(n)%t(k)%inv, nt, &
               stats%minimum%t, stats%maximum%t, stats%average%t, stats%rms_err%t)
            call da_stats_calculate(iv%info(mtgirs)%obs_global_index(n), &
               k, iv%mtgirs(n)%q(k)%qc, iv%mtgirs(n)%q(k)%inv, nq, &
               stats%minimum%q, stats%maximum%q, stats%average%q, stats%rms_err%q)
         end if
      end do
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)
   call da_proc_sum_int(nq)

   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)
   call da_proc_stats_combine(stats%average%t, stats%rms_err%t, &
      stats%minimum%t%value, stats%maximum%t%value, &
      stats%minimum%t%n, stats%maximum%t%n, &
      stats%minimum%t%l, stats%maximum%t%l)
   call da_proc_stats_combine(stats%average%q, stats%rms_err%q, &
      stats%minimum%q%value, stats%maximum%q%value, &
      stats%minimum%q%n, stats%maximum%q%n, &
      stats%minimum%q%l, stats%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for mtgirs'
         call da_print_stats_mtgirs(stats_unit, nu, nv, nt, nq, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_mtgirs")

end subroutine da_oi_stats_mtgirs


