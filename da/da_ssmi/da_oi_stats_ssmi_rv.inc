subroutine da_oi_stats_ssmi_rv(stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   integer                          :: ntpw, nspeed, n
   type (stats_ssmi_retrieval_type) :: ssmi_retrieval

   ntpw = 0
   nspeed = 0

   ssmi_retrieval%maximum%tpw   = maxmin_type(missing_r, 0, 0)
   ssmi_retrieval%maximum%speed = maxmin_type(missing_r, 0, 0)
   ssmi_retrieval%minimum%tpw   = maxmin_type(-missing_r, 0, 0)
   ssmi_retrieval%minimum%speed = maxmin_type(-missing_r, 0, 0)
   ssmi_retrieval%average = residual_ssmi_retrieval_type(0.0, 0.0)
   ssmi_retrieval%rms_err = ssmi_retrieval%average

   if (oi%num_ssmi_retrieval .gt. 0) then
      do n=1, oi%num_ssmi_retrieval
         if (oi%ssmi_retrieval(n)%loc%proc_domain) then
            call da_stats_calculate(oi%ssmi_retrieval(n)%loc%obs_global_index, &
                              0, oi%ssmi_retrieval(n)%tpw%qc, &
                              oi%ssmi_retrieval(n)%tpw%inv, ntpw, &
                              ssmi_retrieval%minimum%tpw, &
                              ssmi_retrieval%maximum%tpw,  &
                              ssmi_retrieval%average%tpw, &
                              ssmi_retrieval%rms_err%tpw)

           call da_stats_calculate(oi%ssmi_retrieval(n)%loc%obs_global_index, &
                              0, oi%ssmi_retrieval(n)%speed%qc, &
                              oi%ssmi_retrieval(n)%speed%inv, nspeed, &
                              ssmi_retrieval%minimum%speed, &
                              ssmi_retrieval%maximum%speed, &
                              ssmi_retrieval%average%speed, &
                              ssmi_retrieval%rms_err%speed)
         end if    ! end if (oi%ssmi_retrieval(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(ntpw)
   call da_proc_sum_int(nspeed)

   call da_proc_stats_combine(ssmi_retrieval%average%tpw, ssmi_retrieval%rms_err%tpw, &
              ssmi_retrieval%minimum%tpw%value, ssmi_retrieval%maximum%tpw%value, &
              ssmi_retrieval%minimum%tpw%n, ssmi_retrieval%maximum%tpw%n, &
              ssmi_retrieval%minimum%tpw%l, ssmi_retrieval%maximum%tpw%l)

   call da_proc_stats_combine(ssmi_retrieval%average%speed, ssmi_retrieval%rms_err%speed, &
              ssmi_retrieval%minimum%speed%value, ssmi_retrieval%maximum%speed%value, &
              ssmi_retrieval%minimum%speed%n, ssmi_retrieval%maximum%speed%n, &
              ssmi_retrieval%minimum%speed%l, ssmi_retrieval%maximum%speed%l)

   if (rootproc) then
      if (ntpw > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           tpw(cm)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntpw
         write(unit=stats_unit, fmt='(a, f12.4,i7)') &
                       ' Minimum(n): ', ssmi_retrieval%minimum%tpw%value, &
                                        ssmi_retrieval%minimum%tpw%n    , &
                       ' Maximum(n): ', ssmi_retrieval%maximum%tpw%value, &
                                        ssmi_retrieval%maximum%tpw%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_retrieval%average%tpw/real(ntpw), &
                       '    RMSE   : ', sqrt(ssmi_retrieval%rms_err%tpw/real(ntpw))
      end if

      if (nspeed > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           speed(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', nspeed
         write(unit=stats_unit, fmt='(a, f12.4,i7)') &
                       ' Minimum(n): ', ssmi_retrieval%minimum%speed%value, &
                                        ssmi_retrieval%minimum%speed%n    , &
                       ' Maximum(n): ', ssmi_retrieval%maximum%speed%value, &
                                        ssmi_retrieval%maximum%speed%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_retrieval%average%speed/real(nspeed), &
                       '    RMSE   : ', sqrt(ssmi_retrieval%rms_err%speed/real(nspeed))
      end if
   end if

end subroutine da_oi_stats_ssmi_rv


