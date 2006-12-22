subroutine da_oi_stats_airsr ( stats_unit, oi )
!-------------------------------------------------------------------------
!  History:
!
!     Creation:        03/07/2006                Syed RH Rizvi        
!
!     Purpose:
!               Print's out innov diagnostics for AIRS retrievals  
!-------------------------------------------------------------------------

  implicit none

  integer,        intent (in)      :: stats_unit    ! Output unit for stats.
  type (ob_type), intent (in)      :: oi            ! OI

  type (stats_airsr_type)          :: airsr
  integer                          :: nt, nq
  integer                          :: n, k

  nt = 0
  nq = 0

  airsr%maximum%t = maxmin_type( missing_r, 0, 0)
  airsr%maximum%q = maxmin_type( missing_r, 0, 0)
  airsr%minimum%t = maxmin_type(-missing_r, 0, 0)
  airsr%minimum%q = maxmin_type(-missing_r, 0, 0)
  airsr%average = residual_airsr1_type(0.0, 0.0)
  airsr%rms_err = airsr%average

  if ( oi%num_airsr > 0 ) then
    do n=1, oi%num_airsr
      if( oi%airsr(n)%loc%proc_domain ) then
        do k=1, oi%airsr(n)%info%levels

          CALl da_stats_calculate( oi%airsr(n)%loc%obs_global_index, &
                              k, oi%airsr(n)%t(k)%qc, &
                              oi%airsr(n)%t(k)%inv, nt, &
                              airsr%minimum%t, airsr%maximum%t, &
                              airsr%average%t, airsr%rms_err%t)
          CALl da_stats_calculate( oi%airsr(n)%loc%obs_global_index, &
                              k, oi%airsr(n)%q(k)%qc, &
                              oi%airsr(n)%q(k)%inv, nq, &
                              airsr%minimum%q, airsr%maximum%q, &
                              airsr%average%q, airsr%rms_err%q)
        end do
      end if    ! end if( oi%airsr(n)%loc%proc_domain )
    end do
  end if

  ! Do inter-processor communication to gather statistics.
  call da_proc_sum_int( nt )
  call da_proc_sum_int( nq )

  call da_proc_stats_combine(airsr%average%t, airsr%rms_err%t, &
                          airsr%minimum%t%value, airsr%maximum%t%value, &
                          airsr%minimum%t%n, airsr%maximum%t%n, &
                          airsr%minimum%t%l, airsr%maximum%t%l )
  call da_proc_stats_combine(airsr%average%q, airsr%rms_err%q, &
                          airsr%minimum%q%value, airsr%maximum%q%value, &
                          airsr%minimum%q%n, airsr%maximum%q%n, &
                          airsr%minimum%q%l, airsr%maximum%q%l )

  if ( rootproc ) then
    if (nt /= 0 .or. nq /= 0) then
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for AIRS retrievals'
      call da_print_stats_airsr( stats_unit, nt, nq, airsr )
    end if
  end if

end subroutine da_oi_stats_airsr


