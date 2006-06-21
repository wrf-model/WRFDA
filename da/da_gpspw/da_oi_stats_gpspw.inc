subroutine da_oi_stats_gpspw ( stats_unit, oi )

  IMPLICIT NONE

  INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
  TYPE (ob_type), INTENT (in)      :: oi            ! OI

  TYPE (stats_gpspw_type)                :: Gpspw
  INTEGER                          :: ntpw
  INTEGER                          :: n

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  ntpw = 0

  Gpspw%maximum%tpw = maxmin_type( missing_r, 0, 0)
  Gpspw%minimum%tpw = maxmin_type(-missing_r, 0, 0)
  Gpspw%average = residual_gpspw1_type(0.0)
  Gpspw%rms_err = Gpspw%average

  IF ( oi%num_Gpspw > 0 ) THEN
    DO n=1, oi%num_Gpspw
      if( oi%gpspw(n)%loc%proc_domain ) then
        call da_stats_calculate( oi%Gpspw(n)%loc%obs_global_index, &
                              0, oi%gpspw(n)%tpw%qc, &
                             oi%gpspw(n)%tpw%inv, ntpw, &
                             gpspw%minimum%tpw  , gpspw%maximum%tpw, &
                             gpspw%average%tpw  , gpspw%rms_err%tpw)

      endif    ! end if( oi%gpspw(n)%loc%proc_domain )
    END DO
  END IF

  !--Do inter-processor communication to gather statistics.

  call da_proc_sum_int( ntpw )

  call da_proc_stats_combine(Gpspw%average%tpw, Gpspw%rms_err%tpw, &
                          Gpspw%minimum%tpw%value, Gpspw%maximum%tpw%value, &
                          Gpspw%minimum%tpw%n, Gpspw%maximum%tpw%n, &
                          Gpspw%minimum%tpw%l, Gpspw%maximum%tpw%l )

  IF ( wrf_dm_on_monitor() ) THEN
    IF (ntpw /= 0) THEN
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Gpspw'
      call da_print_stats_gpspw( stats_unit, ntpw, Gpspw )
    END IF
  END IF

end subroutine da_oi_stats_gpspw

