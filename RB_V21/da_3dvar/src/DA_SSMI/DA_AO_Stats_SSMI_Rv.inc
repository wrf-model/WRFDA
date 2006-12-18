SUBROUTINE DA_AO_Stats_SSMI_Rv( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (IN)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (IN)      :: oi            ! OI
   TYPE ( y_type), INTENT (IN)      :: re            ! A - O

   INTEGER                          :: ntpw, nspeed, n
   TYPE (stats_ssmi_retrieval_type) :: ssmi_retrieval

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ntpw = 0
   nspeed = 0
   num_ssmi_rv_tot = 0

   ssmi_retrieval%maximum%tpw   = maxmin_type( missing_r, 0, 0)
   ssmi_retrieval%maximum%speed = maxmin_type( missing_r, 0, 0)
   ssmi_retrieval%minimum%tpw   = maxmin_type(-missing_r, 0, 0)
   ssmi_retrieval%minimum%speed = maxmin_type(-missing_r, 0, 0)
   ssmi_retrieval%average = residual_ssmi_retrieval_type(0.0, 0.0)
   ssmi_retrieval%rms_err = ssmi_retrieval%average

   IF ( oi%num_ssmi_retrieval .gt. 0) THEN

     DO n=1, oi%num_ssmi_retrieval
  
      if( oi%ssmi_retrieval(n)%loc%proc_domain ) then
  
        CALL  Stats_Calculate( n, 0, oi%ssmi_retrieval(n)%tpw%qc, &
                               re%ssmi_retrieval(n)%tpw, ntpw,  &
                    ssmi_retrieval%minimum%tpw,  ssmi_retrieval%maximum%tpw,&
                    ssmi_retrieval%average%tpw,  ssmi_retrieval%rms_err%tpw)

        CALL  Stats_Calculate( n, 0, oi%ssmi_retrieval(n)%Speed%qc, &
                               re%ssmi_retrieval(n)%Speed,    nspeed,  &
                    ssmi_retrieval%minimum%Speed,  ssmi_retrieval%maximum%Speed,&
                    ssmi_retrieval%average%Speed,  ssmi_retrieval%rms_err%Speed)

      endif    ! end if( oi%ssmi_retrieval(n)%loc%proc_domain )

     END DO
     
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( ntpw )
   call proc_sum_int( nspeed )
   num_ssmi_rv_tot = ntpw + nspeed
   
   call proc_stats_combine(ssmi_retrieval%average%tpw, ssmi_retrieval%rms_err%tpw, &
              ssmi_retrieval%minimum%tpw%value, ssmi_retrieval%maximum%tpw%value, &
              ssmi_retrieval%minimum%tpw%n, ssmi_retrieval%maximum%tpw%n, &
              ssmi_retrieval%minimum%tpw%l, ssmi_retrieval%maximum%tpw%l )

   call proc_stats_combine(ssmi_retrieval%average%speed, ssmi_retrieval%rms_err%speed, &
              ssmi_retrieval%minimum%speed%value, ssmi_retrieval%maximum%speed%value, &
              ssmi_retrieval%minimum%speed%n, ssmi_retrieval%maximum%speed%n, &
              ssmi_retrieval%minimum%speed%l, ssmi_retrieval%maximum%speed%l )

   IF ( wrf_dm_on_monitor() ) THEN

    IF ( ntpw /= 0 )THEN

      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_retrieval'
      write(unit=stats_unit, fmt='(a/)') '   var           tpw(cm)     n'
      write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntpw
      write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                       ' Minimum(n): ', ssmi_retrieval%minimum%tpw%value, &
                                        ssmi_retrieval%minimum%tpw%n    , &
                       ' Maximum(n): ', ssmi_retrieval%maximum%tpw%value, &
                                        ssmi_retrieval%maximum%tpw%n
      write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_retrieval%average%tpw/real(ntpw), &
                       '    RMSE   : ', sqrt(ssmi_retrieval%rms_err%tpw/real(ntpw))
    END IF

    IF ( nspeed /= 0 ) THEN

      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_retrieval'
      write(unit=stats_unit, fmt='(a/)') '   var           Speed(m/s)     n'
      write(unit=stats_unit, fmt='(a,i14)') '  Number: ', nspeed
      write(unit=stats_unit, fmt='(a, f12.4,i5)') &
                       ' Minimum(n): ', ssmi_retrieval%minimum%Speed%value, &
                                        ssmi_retrieval%minimum%Speed%n    , &
                       ' Maximum(n): ', ssmi_retrieval%maximum%speed%value, &
                                        ssmi_retrieval%maximum%speed%n
      write(unit=stats_unit, fmt='(a, f12.4,5x)') &
                       ' Average   : ', ssmi_retrieval%average%Speed/real(nspeed), &
                       '    RMSE   : ', sqrt(ssmi_retrieval%rms_err%Speed/real(nspeed))
    END IF

   END IF

END SUBROUTINE DA_AO_Stats_SSMI_Rv

