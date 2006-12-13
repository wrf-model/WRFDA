subroutine da_oi_stats_sound ( stats_unit, oi )

  implicit none

  integer,        intent (in)      :: stats_unit    ! Output unit for stats.
  type (ob_type), intent (in)      :: oi            ! OI

  type (stats_sound_type)          :: Sound
  integer                          :: nu, nv, nt, nq
  integer                          :: n, k

  if (trace_use) call da_trace_entry("da_oi_stats_sound")

  if (trace_use) call da_trace_entry("da_oi_stats_sound")

  nu = 0
  nv = 0
  nt = 0
  nq = 0

  Sound%maximum%u = maxmin_type( missing_r, 0, 0)
  Sound%maximum%v = maxmin_type( missing_r, 0, 0)
  Sound%maximum%t = maxmin_type( missing_r, 0, 0)
  Sound%maximum%q = maxmin_type( missing_r, 0, 0)
  Sound%minimum%u = maxmin_type(-missing_r, 0, 0)
  Sound%minimum%v = maxmin_type(-missing_r, 0, 0)
  Sound%minimum%t = maxmin_type(-missing_r, 0, 0)
  Sound%minimum%q = maxmin_type(-missing_r, 0, 0)
  Sound%average = residual_sound1_type(0.0, 0.0, 0.0, 0.0)
  Sound%rms_err = Sound%average

  if ( oi%num_Sound > 0 ) then
    do n=1, oi%num_Sound
      if( oi%sound(n)%loc%proc_domain ) then
        do k=1, oi%sound(n)%info%levels

          CALl da_stats_calculate( oi%Sound(n)%loc%obs_global_index, &
                              k, oi%Sound(n)%u(k)%qc, &
                              oi%sound(n)%u(k)%inv, nu, &
                              sound%minimum%u, Sound%maximum%u, &
                              sound%average%u, Sound%rms_err%u)
          CALl da_stats_calculate( oi%Sound(n)%loc%obs_global_index, &
                              k, oi%Sound(n)%v(k)%qc, &
                              oi%sound(n)%v(k)%inv, nv, &
                              sound%minimum%v, Sound%maximum%v, &
                              sound%average%v, Sound%rms_err%v)
          CALl da_stats_calculate( oi%Sound(n)%loc%obs_global_index, &
                              k, oi%Sound(n)%t(k)%qc, &
                              oi%sound(n)%t(k)%inv, nt, &
                              sound%minimum%t, Sound%maximum%t, &
                              sound%average%t, Sound%rms_err%t)
          CALl da_stats_calculate( oi%Sound(n)%loc%obs_global_index, &
                              k, oi%Sound(n)%q(k)%qc, &
                              oi%Sound(n)%q(k)%inv, nq, &
                              Sound%minimum%q, Sound%maximum%q, &
                              Sound%average%q, Sound%rms_err%q)
        end do
      end if    ! end if( oi%sound(n)%loc%proc_domain )
    end do
  end if

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nu )
  call da_proc_sum_int( nv )
  call da_proc_sum_int( nt )
  call da_proc_sum_int( nq )

  call da_proc_stats_combine(Sound%average%u, Sound%rms_err%u, &
                          Sound%minimum%u%value, Sound%maximum%u%value, &
                          Sound%minimum%u%n, Sound%maximum%u%n, &
                          Sound%minimum%u%l, Sound%maximum%u%l )
  call da_proc_stats_combine(Sound%average%v, Sound%rms_err%v, &
                          Sound%minimum%v%value, Sound%maximum%v%value, &
                          Sound%minimum%v%n, Sound%maximum%v%n, &
                          Sound%minimum%v%l, Sound%maximum%v%l )
  call da_proc_stats_combine(Sound%average%t, Sound%rms_err%t, &
                          Sound%minimum%t%value, Sound%maximum%t%value, &
                          Sound%minimum%t%n, Sound%maximum%t%n, &
                          Sound%minimum%t%l, Sound%maximum%t%l )
  call da_proc_stats_combine(Sound%average%q, Sound%rms_err%q, &
                          Sound%minimum%q%value, Sound%maximum%q%value, &
                          Sound%minimum%q%n, Sound%maximum%q%n, &
                          Sound%minimum%q%l, Sound%maximum%q%l )

  if ( rootproc ) then
    if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. nq /= 0) then
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Sound'
      call da_print_stats_sound( stats_unit, nu, nv, nt, nq, Sound )
    end if
  end if

  if (trace_use) call da_trace_exit("da_oi_stats_sound")

  if (trace_use) call da_trace_exit("da_oi_stats_sound")

end subroutine da_oi_stats_sound

