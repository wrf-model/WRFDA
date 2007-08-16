subroutine da_oi_stats_radar (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in) :: oi            ! OI

   type (stats_radar_type) :: stats
   integer                 :: nrv, nrf
   integer                 :: n, k

   if (trace_use) call da_trace_entry("da_oi_stats_radar")

   nrv = 0
   nrf = 0
      
   stats%maximum%rv = maxmin_type(missing_r, 0, 0)
   stats%minimum%rv = maxmin_type(-missing_r, 0, 0)
   stats%maximum%rf = maxmin_type(missing_r, 0, 0)
   stats%minimum%rf = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_radar1_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, oi%nlocal(radar)
      if (oi%radar(n)%loc%proc_domain) then
         do k=1, oi%radar(n)%info%levels
            if (use_radar_rv) then
               call da_stats_calculate(oi%radar(n)%loc%obs_global_index, &
                  k, oi%radar(n)%rv(k)%qc, &
                  oi%radar(n)%rv(k)%inv, nrv, &
                  stats%minimum%rv, stats%maximum%rv, &
                  stats%average%rv, stats%rms_err%rv)
            end if

            if (use_radar_rf) then
               call da_stats_calculate(oi%radar(n)%loc%obs_global_index, &
                  k, oi%radar(n)%rf(k)%qc, &
                  oi%radar(n)%rf(k)%inv, nrf, &
                  stats%minimum%rf, stats%maximum%rf, &
                  stats%average%rf, stats%rms_err%rf)
            end if
         end do
      end if
   end do

   ! Do inter-processor communication to gather statistics.

   if (use_radar_rv) then
      call da_proc_sum_int(nrv)
      call da_proc_stats_combine(stats%average%rv, stats%rms_err%rv, &
         stats%minimum%rv%value, stats%maximum%rv%value, &
         stats%minimum%rv%n, stats%maximum%rv%n, &
         stats%minimum%rv%l, stats%maximum%rv%l)
   end if
   
   if (use_radar_rf) then
      call da_proc_sum_int(nrf)
      call da_proc_stats_combine(stats%average%rf, stats%rms_err%rf, &
         stats%minimum%rf%value, stats%maximum%rf%value, &
         stats%minimum%rf%n, stats%maximum%rf%n, &
         stats%minimum%rf%l, stats%maximum%rf%l)
   end if

   if (rootproc) then
      if (nrv /= 0 .or. nrf /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for radar'
         call da_print_stats_radar(stats_unit, nrv, nrf, stats)
      end if
   end if

   if (trace_use) call da_trace_exit("da_oi_stats_radar")

end subroutine da_oi_stats_radar


