SUBROUTINE da_oi_stats_pilot ( stats_unit, oi )

  IMPLICIT NONE

  INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
  TYPE (ob_type), INTENT (in)      :: oi            ! OI

  TYPE (stats_pilot_type)          :: Pilot
  INTEGER                          :: nu, nv
  INTEGER                          :: n, k

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  nu = 0
  nv = 0

  Pilot%maximum%u = maxmin_type( missing_r, 0, 0)
  Pilot%maximum%v = maxmin_type( missing_r, 0, 0)
  Pilot%minimum%u = maxmin_type(-missing_r, 0, 0)
  Pilot%minimum%v = maxmin_type(-missing_r, 0, 0)
  Pilot%average = residual_pilot1_type(0.0, 0.0)
  Pilot%rms_err = Pilot%average

  IF ( oi%num_Pilot > 0 ) THEN
    DO n=1, oi%num_Pilot
      if( oi%pilot(n)%loc%proc_domain ) then
        do k=1, oi%Pilot(n)%info%levels
          CALL da_stats_calculate( oi%Pilot(n)%loc%obs_global_index, &
                              k, oi%pilot(n)%u(k)%qc,  &
                              oi%pilot(n)%u(k)%inv, nu, &
                              pilot%minimum%u, Pilot%maximum%u, &
                              pilot%average%u, Pilot%rms_err%u)
          CALL da_stats_calculate( oi%Pilot(n)%loc%obs_global_index, &
                              k, oi%Pilot(n)%v(k)%qc, &
                              oi%Pilot(n)%v(k)%inv, nv, &
                              Pilot%minimum%v, Pilot%maximum%v, &
                              Pilot%average%v, Pilot%rms_err%v)
        end do
      endif    ! end if( oi%pilot(n)%loc%proc_domain )
    END DO
  END IF

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nu )
  call da_proc_sum_int( nv )

  call da_proc_stats_combine(Pilot%average%u, Pilot%rms_err%u, &
                          Pilot%minimum%u%value, Pilot%maximum%u%value, &
                          Pilot%minimum%u%n, Pilot%maximum%u%n, &
                          Pilot%minimum%u%l, Pilot%maximum%u%l )
  call da_proc_stats_combine(Pilot%average%v, Pilot%rms_err%v, &
                          Pilot%minimum%v%value, Pilot%maximum%v%value, &
                          Pilot%minimum%v%n, Pilot%maximum%v%n, &
                          Pilot%minimum%v%l, Pilot%maximum%v%l )

  IF ( wrf_dm_on_monitor() ) THEN
    IF (nu /= 0 .or. nv /= 0 ) THEN
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Pilot'
      CALL da_print_stats_pilot( stats_unit, nu, nv, Pilot )
    END IF
  END IF

END SUBROUTINE da_oi_stats_pilot

