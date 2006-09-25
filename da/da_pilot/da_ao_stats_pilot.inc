subroutine da_ao_stats_pilot( stats_unit, oi, re )

  IMPLICIT NONE

  INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
  TYPE (ob_type), INTENT (in)      :: oi            ! OI
  TYPE ( y_type), INTENT (in)      :: re            ! A - O

  TYPE (stats_pilot_type)          :: Pilot
  INTEGER                          :: nu, nv
  INTEGER                          :: n, k

  nu = 0
  nv = 0
  num_pilot_tot = 0

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
          call da_stats_calculate( n, k, oi%Pilot(n)%u(k)%qc, & 
                              re%pilot(n)%u(k), nu, & 
                              pilot%minimum%u, Pilot%maximum%u, &
                              pilot%average%u, Pilot%rms_err%u )
          call da_stats_calculate( n, k, oi%Pilot(n)%v(k)%qc, & 
                              re%Pilot(n)%v(k), nv, & 
                              Pilot%minimum%v, Pilot%maximum%v, &
                              Pilot%average%v, Pilot%rms_err%v )
        end do
      endif    ! end if( oi%pilot(n)%loc%proc_domain )
    END DO
  END IF

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nu )
  call da_proc_sum_int( nv )
  num_pilot_tot = nu + nv

  call da_proc_stats_combine(Pilot%average%u, Pilot%rms_err%u, &
                          Pilot%minimum%u%value, Pilot%maximum%u%value, &
                          Pilot%minimum%u%n, Pilot%maximum%u%n, &
                          Pilot%minimum%u%l, Pilot%maximum%u%l )
  call da_proc_stats_combine(Pilot%average%v, Pilot%rms_err%v, &
                          Pilot%minimum%v%value, Pilot%maximum%v%value, &
                          Pilot%minimum%v%n, Pilot%maximum%v%n, &
                          Pilot%minimum%v%l, Pilot%maximum%v%l )

  IF ( rootproc ) THEN
    IF ( nu /= 0 .or. nv /= 0 ) THEN
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Pilot'
      call da_print_stats_pilot(stats_unit, nu, nv, Pilot)
    END IF
  END IF

end subroutine da_ao_stats_pilot

