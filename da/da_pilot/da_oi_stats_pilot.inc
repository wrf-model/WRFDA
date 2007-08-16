subroutine da_oi_stats_pilot (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in) :: oi            ! OI

   type (stats_pilot_type) :: stats
   integer                 :: nu, nv
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_pilot")

   nu = 0
   nv = 0

   stats%maximum%u = maxmin_type(missing_r, 0, 0)
   stats%maximum%v = maxmin_type(missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_pilot1_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, oi%nlocal(pilot)
      if (oi%pilot(n)%loc%proc_domain) then
         do k=1, oi%pilot(n)%info%levels
            call da_stats_calculate(oi%pilot(n)%loc%obs_global_index, &
               k, oi%pilot(n)%u(k)%qc,  &
               oi%pilot(n)%u(k)%inv, nu, &
               stats%minimum%u, stats%maximum%u, &
               stats%average%u, stats%rms_err%u)
            call da_stats_calculate(oi%pilot(n)%loc%obs_global_index, &
               k, oi%pilot(n)%v(k)%qc, &
               oi%pilot(n)%v(k)%inv, nv, &
               stats%minimum%v, stats%maximum%v, &
               stats%average%v, stats%rms_err%v)
         end do
      end if
   end do

   ! do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)

   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for pilot'
         call da_print_stats_pilot(stats_unit, nu, nv, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_pilot")

 end subroutine da_oi_stats_pilot


