subroutine da_oi_stats_profiler ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_profiler_type)       :: Profiler
   INTEGER                          :: nu, nv
   INTEGER                          :: n, k

   nu = 0
   nv = 0
   
   Profiler%maximum%u = maxmin_type( missing_r, 0, 0)
   Profiler%maximum%v = maxmin_type( missing_r, 0, 0)
   Profiler%minimum%u = maxmin_type(-missing_r, 0, 0)
   Profiler%minimum%v = maxmin_type(-missing_r, 0, 0)
   Profiler%average = residual_profiler1_type(0.0, 0.0)
   Profiler%rms_err = Profiler%average

   IF ( oi%num_Profiler > 0 ) THEN

      DO n=1, oi%num_Profiler

      if( oi%profiler(n)%loc%proc_domain ) then

       do k=1, oi%Profiler(n)%info%levels
         call da_stats_calculate( oi%profiler(n)%loc%obs_global_index, &
                               k, oi%profiler(n)%u(k)%qc,  &
                               oi%profiler(n)%u(k)%inv, nu, &
                               profiler%minimum%u, Profiler%maximum%u, &
                               profiler%average%u, Profiler%rms_err%u)
         call da_stats_calculate( oi%profiler(n)%loc%obs_global_index, &
                               k, oi%profiler(n)%v(k)%qc, &
                               oi%profiler(n)%v(k)%inv, nv, &
                               Profiler%minimum%v, Profiler%maximum%v, &
                               Profiler%average%v, Profiler%rms_err%v)
       end do
      endif    ! end if( oi%profiler(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call da_proc_sum_int( nu )
   call da_proc_sum_int( nv )
   
   call da_proc_stats_combine(Profiler%average%u, Profiler%rms_err%u, &
                           Profiler%minimum%u%value, Profiler%maximum%u%value, &
                           Profiler%minimum%u%n, Profiler%maximum%u%n, &
                           Profiler%minimum%u%l, Profiler%maximum%u%l )
   call da_proc_stats_combine(Profiler%average%v, Profiler%rms_err%v, &
                           Profiler%minimum%v%value, Profiler%maximum%v%value, &
                           Profiler%minimum%v%n, Profiler%maximum%v%n, &
                           Profiler%minimum%v%l, Profiler%maximum%v%l )
   
   IF ( rootproc ) THEN
   
    IF (nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Profiler'
   
      call da_print_stats_profiler( stats_unit, nu, nv, Profiler )
   
    END IF
   
   END IF

end subroutine da_oi_stats_profiler

