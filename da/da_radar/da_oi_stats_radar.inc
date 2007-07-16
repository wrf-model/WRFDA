subroutine da_oi_stats_radar (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   type (stats_radar_type)          :: radar
   integer                          :: nrv, nrf
   integer                          :: n, k

   nrv = 0
   nrf = 0
      
   radar%maximum%rv = maxmin_type(missing_r, 0, 0)
   radar%minimum%rv = maxmin_type(-missing_r, 0, 0)
   radar%maximum%rf = maxmin_type(missing_r, 0, 0)
   radar%minimum%rf = maxmin_type(-missing_r, 0, 0)
   radar%average = residual_radar1_type(0.0, 0.0)
   radar%rms_err = radar%average

   if (oi%num_radar > 0) then
      do n=1, oi%num_radar
         if (oi%radar(n)%loc%proc_domain) then
            do k=1, oi%radar(n)%info%levels
               if (use_radar_rv) then
                  call da_stats_calculate(oi%radar(n)%loc%obs_global_index, &
                                     k, oi%radar(n)%rv(k)%qc, &
                                     oi%radar(n)%rv(k)%inv, nrv, &
                                     radar%minimum%rv, radar%maximum%rv, &
                                     radar%average%rv, radar%rms_err%rv)
               end if

               if (use_radar_rf) then
                  call da_stats_calculate(oi%radar(n)%loc%obs_global_index, &
                                     k, oi%radar(n)%rf(k)%qc, &
                                     oi%radar(n)%rf(k)%inv, nrf, &
                                     radar%minimum%rf, radar%maximum%rf, &
                                     radar%average%rf, radar%rms_err%rf)
               end if
            end do
         end if
      end do
   end if

   ! Do inter-processor communication to gather statistics.

   if (use_radar_rv) then
      call da_proc_sum_int(nrv)
      call da_proc_stats_combine(radar%average%rv, radar%rms_err%rv, &
                             radar%minimum%rv%value, radar%maximum%rv%value, &
                             radar%minimum%rv%n, radar%maximum%rv%n, &
                             radar%minimum%rv%l, radar%maximum%rv%l)
   end if
   
   if (use_radar_rf) then
      call da_proc_sum_int(nrf)
      call da_proc_stats_combine(radar%average%rf, radar%rms_err%rf, &
                             radar%minimum%rf%value, radar%maximum%rf%value, &
                             radar%minimum%rf%n, radar%maximum%rf%n, &
                             radar%minimum%rf%l, radar%maximum%rf%l)
   end if

   if (rootproc) then
      if (nrv /= 0 .or. nrf /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for radar'
         call da_print_stats_radar(stats_unit, nrv, nrf, radar)
      end if
   end if

end subroutine da_oi_stats_radar


