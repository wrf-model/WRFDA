SUBROUTINE da_ao_stats_airep( stats_unit, oi, re )

  IMPLICIT NONE

  INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
  TYPE (ob_type), INTENT (in)      :: oi            ! OI
  TYPE ( y_type), INTENT (in)      :: re            ! A - O

  TYPE (stats_airep_type)          :: Airep
  INTEGER                          :: nu, nv, nt
  INTEGER                          :: n, k

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  num_airep_tot = 0

  nu = 0
  nv = 0
  nt = 0

  Airep%maximum%u = maxmin_type( missing_r, 0, 0)
  Airep%maximum%v = maxmin_type( missing_r, 0, 0)
  Airep%maximum%t = maxmin_type( missing_r, 0, 0)
  Airep%minimum%u = maxmin_type(-missing_r, 0, 0)
  Airep%minimum%v = maxmin_type(-missing_r, 0, 0)
  Airep%minimum%t = maxmin_type(-missing_r, 0, 0)
  Airep%average = residual_airep1_type(0.0, 0.0, 0.0)
  Airep%rms_err = Airep%average

  IF ( oi%num_Airep > 0 ) THEN

    DO n=1, oi%num_Airep

      if( oi%airep(n)%loc%proc_domain ) then

        do k=1, oi%Airep(n)%info%levels
          CALL da_stats_calculate( n, k, oi%Airep(n)%u(k)%qc,  & 
                                re%Airep(n)%u(k), nu, &
                                airep%minimum%u, Airep%maximum%u, &
                                airep%average%u, Airep%rms_err%u )
          CALL da_stats_calculate( n, k, oi%Airep(n)%v(k)%qc,  & 
                                re%Airep(n)%v(k), nv, &
                                airep%minimum%v, Airep%maximum%v, &
                                airep%average%v, Airep%rms_err%v )
          CALL da_stats_calculate( n, k, oi%Airep(n)%t(k)%qc,  & 
                                re%Airep(n)%t(k), nt, &
                                 Airep%minimum%t, Airep%maximum%t, &
                                 Airep%average%t, Airep%rms_err%t )
        end do
      endif    ! end if( oi%airep(n)%loc%proc_domain )
    END DO
  END IF

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nu )
  call da_proc_sum_int( nv )
  call da_proc_sum_int( nt )
  num_airep_tot = nu + nv + nt

  call da_proc_stats_combine(Airep%average%u, Airep%rms_err%u, &
                          Airep%minimum%u%value, Airep%maximum%u%value, &
                          Airep%minimum%u%n, Airep%maximum%u%n, &
                          Airep%minimum%u%l, Airep%maximum%u%l )
  call da_proc_stats_combine(Airep%average%v, Airep%rms_err%v, &
                          Airep%minimum%v%value, Airep%maximum%v%value, &
                          Airep%minimum%v%n, Airep%maximum%v%n, &
                          Airep%minimum%v%l, Airep%maximum%v%l )
  call da_proc_stats_combine(Airep%average%t, Airep%rms_err%t, &
                          Airep%minimum%t%value, Airep%maximum%t%value, &
                          Airep%minimum%t%n, Airep%maximum%t%n, &
                          Airep%minimum%t%l, Airep%maximum%t%l )
   
  IF ( wrf_dm_on_monitor() ) THEN
    IF ( nu /= 0 .or. nv /= 0 .or. nt /= 0 ) THEN
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Airep'
      call da_print_stats_airep(stats_unit, nu, nv, nt, Airep)
    END IF
  END IF

END SUBROUTINE da_ao_stats_airep
