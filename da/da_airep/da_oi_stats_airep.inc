subroutine da_oi_stats_airep (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_airep_type) :: stats
   integer                 :: nu, nv, nt
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_airep")

   nu = 0
   nv = 0
   nt = 0

   stats%maximum%u = maxmin_type(missing_r, 0, 0)
   stats%maximum%v = maxmin_type(missing_r, 0, 0)
   stats%maximum%t = maxmin_type(missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_airep1_type(0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(airep)%nlocal
      do k=1, iv%info(airep)%levels(n)
         if (iv%info(airep)%proc_domain(k,n)) then
            call da_stats_calculate(iv%info(airep)%obs_global_index(n), &
               k, iv%airep(n)%u(k)%qc, &
               iv%airep(n)%u(k)%inv, nu, &
               stats%minimum%u, stats%maximum%u, &
               stats%average%u, stats%rms_err%u)
            call da_stats_calculate(iv%info(airep)%obs_global_index(n), &
               k, iv%airep(n)%v(k)%qc, &
               iv%airep(n)%v(k)%inv, nv, &
               stats%minimum%v, stats%maximum%v, &
               stats%average%v, stats%rms_err%v)
            call da_stats_calculate(iv%info(airep)%obs_global_index(n), &
               k, iv%airep(n)%t(k)%qc, &
               iv%airep(n)%t(k)%inv, nt, &
               stats%minimum%t, stats%maximum%t, &
               stats%average%t, stats%rms_err%t)
         end if
      end do
   end do

   ! do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)

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

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for airep'
         call da_print_stats_airep(stats_unit, nu, nv, nt, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_airep")

 end subroutine da_oi_stats_airep


