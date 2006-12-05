subroutine da_oi_stats_satem ( stats_unit, oi )

  implicit none

  integer,        intent (in)      :: stats_unit    ! Output unit for stats.
  type (ob_type), intent (in)      :: oi            ! OI

  type (stats_satem_type)          :: Satem
  integer                          :: nthickness
  integer                          :: n, k

  nthickness = 0

  Satem%maximum%thickness = maxmin_type( missing_r, 0, 0)
  Satem%minimum%thickness = maxmin_type(-missing_r, 0, 0)
  Satem%average = residual_satem1_type(0.0)
  Satem%rms_err = Satem%average

  if ( oi%num_Satem > 0 ) then
    do n=1, oi%num_Satem
      if( oi%satem(n)%loc%proc_domain ) then
        do k=1, oi%satem(n)%info%levels
          call da_stats_calculate( oi%Satem(n)%loc%obs_global_index, &
                         k, oi%Satem(n)%thickness(k)%qc, &
                         oi%Satem(n)%thickness(k)%inv, nthickness, &
                         Satem%minimum%thickness, Satem%maximum%thickness, &
                         Satem%average%thickness, Satem%rms_err%thickness)
        end do
      end if    ! end if( oi%satem(n)%loc%proc_domain )
    end do
  end if

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nthickness )

  call da_proc_stats_combine(Satem%average%thickness, Satem%rms_err%thickness, &
            Satem%minimum%thickness%value, Satem%maximum%thickness%value, &
            Satem%minimum%thickness%n, Satem%maximum%thickness%n, &
            Satem%minimum%thickness%l, Satem%maximum%thickness%l )

  if ( rootproc ) then
    if (nthickness /= 0 ) then
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Satem'
      call da_print_stats_satem( stats_unit, nthickness, Satem )
    end if
  end if

end subroutine da_oi_stats_satem

