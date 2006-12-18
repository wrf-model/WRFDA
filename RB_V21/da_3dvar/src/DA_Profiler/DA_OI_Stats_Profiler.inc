SUBROUTINE DA_OI_Stats_Profiler ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_profiler_type)       :: Profiler
   INTEGER                          :: nu, nv
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

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
         CALL Stats_Calculate( n, k, oi%Profiler(n)%u(k)%qc,  &
                               oi%Profiler(n)%u(k)%inv, nu, &
                               Profiler%minimum%u, Profiler%maximum%u, &
                               Profiler%average%u, Profiler%rms_err%u)
         CALL Stats_Calculate( n, k, oi%Profiler(n)%v(k)%qc, &
                               oi%Profiler(n)%v(k)%inv, nv, &
                               Profiler%minimum%v, Profiler%maximum%v, &
                               Profiler%average%v, Profiler%rms_err%v)
       end do
      endif    ! end if( oi%profiler(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   
   call proc_stats_combine(Profiler%average%u, Profiler%rms_err%u, &
                           Profiler%minimum%u%value, Profiler%maximum%u%value, &
                           Profiler%minimum%u%n, Profiler%maximum%u%n, &
                           Profiler%minimum%u%l, Profiler%maximum%u%l )
   call proc_stats_combine(Profiler%average%v, Profiler%rms_err%v, &
                           Profiler%minimum%v%value, Profiler%maximum%v%value, &
                           Profiler%minimum%v%n, Profiler%maximum%v%n, &
                           Profiler%minimum%v%l, Profiler%maximum%v%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF (nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Profiler'
   
      CALL DA_Print_Stats_Profiler( stats_unit, nu, nv, Profiler )
   
    END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Profiler

