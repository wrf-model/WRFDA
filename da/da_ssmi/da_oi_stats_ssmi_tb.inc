subroutine da_oi_stats_ssmi_tb (stats_unit, oi)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (ob_type), intent (in)      :: oi            ! OI

   integer                          :: ntb19v,ntb19h,ntb22v,ntb37v,ntb37h, &
                                       ntb85v,ntb85h
   integer                          :: n
   type (stats_ssmi_tb_type)        :: ssmi_tb

   ntb19v = 0
   ntb19h = 0
   ntb22v = 0
   ntb37v = 0
   ntb37h = 0
   ntb85v = 0
   ntb85h = 0

   ssmi_tb%maximum%tb19v = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb19h = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb22v = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb37v = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb37h = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb85v = maxmin_type(missing_r, 0, 0)
   ssmi_tb%maximum%tb85h = maxmin_type(missing_r, 0, 0)
   ssmi_tb%minimum%tb19v = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb19h = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb22v = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb37v = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb37h = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb85v = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%minimum%tb85h = maxmin_type(-missing_r, 0, 0)
   ssmi_tb%average = residual_ssmi_tb_type(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
   ssmi_tb%rms_err = ssmi_tb%average

   if (oi%num_ssmi_tb .gt. 0) then
      do n=1, oi%num_ssmi_tb

         if (oi%ssmi_tb(n)%loc%proc_domain) then

            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb19v%qc, & 
                                  oi%ssmi_tb(n)%tb19v%inv, ntb19v, &
                                  ssmi_tb%minimum%tb19v, ssmi_tb%maximum%tb19v,&
                                  ssmi_tb%average%tb19v, ssmi_tb%rms_err%tb19v)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb19h%qc, & 
                                  oi%ssmi_tb(n)%tb19h%inv, ntb19h, &
                                  ssmi_tb%minimum%tb19h, ssmi_tb%maximum%tb19h,&
                                  ssmi_tb%average%tb19h, ssmi_tb%rms_err%tb19h)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb22v%qc, & 
                                  oi%ssmi_tb(n)%tb22v%inv, ntb22v, &
                                  ssmi_tb%minimum%tb22v, ssmi_tb%maximum%tb22v,&
                                  ssmi_tb%average%tb22v, ssmi_tb%rms_err%tb22v)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb37v%qc, & 
                                  oi%ssmi_tb(n)%tb37v%inv, ntb37v, &
                                  ssmi_tb%minimum%tb37v, ssmi_tb%maximum%tb37v,&
                                  ssmi_tb%average%tb37v, ssmi_tb%rms_err%tb37v)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb37h%qc, & 
                                  oi%ssmi_tb(n)%tb37h%inv, ntb37h, &
                                  ssmi_tb%minimum%tb37h, ssmi_tb%maximum%tb37h,&
                                  ssmi_tb%average%tb37h, ssmi_tb%rms_err%tb37h)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb85v%qc, & 
                                  oi%ssmi_tb(n)%tb85v%inv, ntb85v, &
                                  ssmi_tb%minimum%tb85v, ssmi_tb%maximum%tb85v,&
                                  ssmi_tb%average%tb85v, ssmi_tb%rms_err%tb85v)
            call da_stats_calculate(oi%ssmi_tb(n)%loc%obs_global_index, &
                                  0, oi%ssmi_tb(n)%tb85h%qc, & 
                                  oi%ssmi_tb(n)%tb85h%inv, ntb85h, &
                                  ssmi_tb%minimum%tb85h, ssmi_tb%maximum%tb85h,&
                                  ssmi_tb%average%tb85h, ssmi_tb%rms_err%tb85h)

         end if    ! end if (oi%ssmi_tb(n)%loc%proc_domain)
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(ntb19v)
   call da_proc_sum_int(ntb19h)
   call da_proc_sum_int(ntb22v)
   call da_proc_sum_int(ntb37v)
   call da_proc_sum_int(ntb37h)
   call da_proc_sum_int(ntb85v)
   call da_proc_sum_int(ntb85h)

   call da_proc_stats_combine(ssmi_tb%average%tb19v, ssmi_tb%rms_err%tb19v, &
              ssmi_tb%minimum%tb19v%value, ssmi_tb%maximum%tb19v%value, &
              ssmi_tb%minimum%tb19v%n, ssmi_tb%maximum%tb19v%n, &
              ssmi_tb%minimum%tb19v%l, ssmi_tb%maximum%tb19v%l)

   call da_proc_stats_combine(ssmi_tb%average%tb19h, ssmi_tb%rms_err%tb19h, &
              ssmi_tb%minimum%tb19h%value, ssmi_tb%maximum%tb19h%value, &
              ssmi_tb%minimum%tb19h%n, ssmi_tb%maximum%tb19h%n, &
              ssmi_tb%minimum%tb19h%l, ssmi_tb%maximum%tb19h%l)

   call da_proc_stats_combine(ssmi_tb%average%tb22v, ssmi_tb%rms_err%tb22v, &
              ssmi_tb%minimum%tb22v%value, ssmi_tb%maximum%tb22v%value, &
              ssmi_tb%minimum%tb22v%n, ssmi_tb%maximum%tb22v%n, &
              ssmi_tb%minimum%tb22v%l, ssmi_tb%maximum%tb22v%l)

   call da_proc_stats_combine(ssmi_tb%average%tb37v, ssmi_tb%rms_err%tb37v, &
              ssmi_tb%minimum%tb37v%value, ssmi_tb%maximum%tb37v%value, &
              ssmi_tb%minimum%tb37v%n, ssmi_tb%maximum%tb37v%n, &
              ssmi_tb%minimum%tb37v%l, ssmi_tb%maximum%tb37v%l)

   call da_proc_stats_combine(ssmi_tb%average%tb37h, ssmi_tb%rms_err%tb37h, &
              ssmi_tb%minimum%tb37h%value, ssmi_tb%maximum%tb37h%value, &
              ssmi_tb%minimum%tb37h%n, ssmi_tb%maximum%tb37h%n, &
              ssmi_tb%minimum%tb37h%l, ssmi_tb%maximum%tb37h%l)

   call da_proc_stats_combine(ssmi_tb%average%tb85v, ssmi_tb%rms_err%tb85v, &
              ssmi_tb%minimum%tb85v%value, ssmi_tb%maximum%tb85v%value, &
              ssmi_tb%minimum%tb85v%n, ssmi_tb%maximum%tb85v%n, &
              ssmi_tb%minimum%tb85v%l, ssmi_tb%maximum%tb85v%l)

   call da_proc_stats_combine(ssmi_tb%average%tb85h, ssmi_tb%rms_err%tb85h, &
              ssmi_tb%minimum%tb85h%value, ssmi_tb%maximum%tb85h%value, &
              ssmi_tb%minimum%tb85h%n, ssmi_tb%maximum%tb85h%n, &
              ssmi_tb%minimum%tb85h%l, ssmi_tb%maximum%tb85h%l)

   if (rootproc) then
      if (ntb19v > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb19v(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb19v
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb19v%value, &
                                          ssmi_tb%minimum%tb19v%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb19v%value, &
                                          ssmi_tb%maximum%tb19v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb19v/real(ntb19v), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb19v/real(ntb19v))
      end if

      if (ntb19h > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb19h(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb19h
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb19h%value, &
                                          ssmi_tb%minimum%tb19h%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb19h%value, &
                                          ssmi_tb%maximum%tb19h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb19h/real(ntb19h), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb19h/real(ntb19h))
      end if

      if (ntb22v > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb22v(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb22v
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb22v%value, &
                                          ssmi_tb%minimum%tb22v%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb22v%value, &
                                          ssmi_tb%maximum%tb22v%n
          write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb22v/real(ntb22v), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb22v/real(ntb22v))
      end if

      if (ntb37v > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb37v(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb37v
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb37v%value, &
                                          ssmi_tb%minimum%tb37v%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb37v%value, &
                                          ssmi_tb%maximum%tb37v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb37v/real(ntb37v), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb37v/real(ntb37v))
      end if

      if (ntb37h > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb37h(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb37h
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb37h%value, &
                                          ssmi_tb%minimum%tb37h%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb37h%value, &
                                         ssmi_tb%maximum%tb37h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb37h/real(ntb37h), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb37h/real(ntb37h))
      end if

      if (ntb85v > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb85v(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb85v
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb85v%value, &
                                          ssmi_tb%minimum%tb85v%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb85v%value, &
                                          ssmi_tb%maximum%tb85v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb85v/real(ntb85v), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb85v/real(ntb85v))
      end if

      if (ntb85h > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_tb'
         write(unit=stats_unit, fmt='(a/)') '   var           tb85h(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb85h
         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                         ' Minimum(n): ', ssmi_tb%minimum%tb85h%value, &
                                          ssmi_tb%minimum%tb85h%n    , &
                         ' Maximum(n): ', ssmi_tb%maximum%tb85h%value, &
                                          ssmi_tb%maximum%tb85h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_tb%average%tb85h/real(ntb85h), &
                       '    RMSE   : ', sqrt(ssmi_tb%rms_err%tb85h/real(ntb85h))
      end if
   end if

end subroutine da_oi_stats_ssmi_tb


