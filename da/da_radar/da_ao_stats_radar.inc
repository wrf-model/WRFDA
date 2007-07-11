subroutine da_ao_stats_radar (stats_unit, oi, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI
   type  (y_type), intent (in)      :: re            ! A - O

   type (stats_radar_type)          :: radar
   integer                          :: nrv, nrf
   integer                          :: n, k

   nrv = 0
   nrf = 0
   num_radar_tot = 0.

   radar%maximum%rv = maxmin_type (missing_r, 0, 0)
   radar%maximum%rf = maxmin_type (missing_r, 0, 0)
   radar%minimum%rv = maxmin_type(-missing_r, 0, 0)
   radar%minimum%rf = maxmin_type(-missing_r, 0, 0)
   radar%average = residual_radar1_type(0.0, 0.0)
   radar%rms_err = radar%average

   if (oi%num_radar > 0) then
      do n=1, oi%num_radar
         if (oi%radar(n)%loc%proc_domain) then
            do k=1, oi%radar(n)%info%levels
               if (use_radar_rv) then
                  call da_stats_calculate (n, k, oi%radar(n)%rv(k)%qc, & 
                                     re%radar(n)%rv(k), nrv, & 
                                     radar%minimum%rv, radar%maximum%rv, &
                                     radar%average%rv, radar%rms_err%rv)
               end if

               if (use_radar_rf) then
                  call da_stats_calculate (n, k, oi%radar(n)%rf(k)%qc, & 
                                     re%radar(n)%rf(k), nrf, & 
                                     radar%minimum%rf, radar%maximum%rf, &
                                     radar%average%rf, radar%rms_err%rf)
               end if
            end do
         end if     ! end if (oi%pilot(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   if (use_radar_rv) then
      call da_proc_sum_int (nrv)
      call da_proc_stats_combine(radar%average%rv, radar%rms_err%rv, &
                             radar%minimum%rv%value, radar%maximum%rv%value, &
                             radar%minimum%rv%n, radar%maximum%rv%n, &
                             radar%minimum%rv%l, radar%maximum%rv%l)
   end if

   if (use_radar_rf) then
      call da_proc_sum_int (nrf)
      call da_proc_stats_combine(radar%average%rf, radar%rms_err%rf, &
                             radar%minimum%rf%value, radar%maximum%rf%value, &
                             radar%minimum%rf%n, radar%maximum%rf%n, &
                             radar%minimum%rf%l, radar%maximum%rf%l)
   end if
   num_radar_tot = nrv + nrf

   if (rootproc) then
      if (nrv /= 0 .or. nrf /= 0) then 
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for radar'
         call da_print_stats_radar(stats_unit, nrv, nrf, radar)
      end if
   end if

end subroutine da_ao_stats_radar


