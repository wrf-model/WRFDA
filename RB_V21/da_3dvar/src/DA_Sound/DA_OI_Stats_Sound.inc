SUBROUTINE DA_OI_Stats_Sound ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_sound_type)          :: Sound
   INTEGER                          :: nu, nv, nt, nq
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

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

   IF ( oi%num_Sound > 0 ) THEN

      DO n=1, oi%num_Sound

      if( oi%sound(n)%loc%proc_domain ) then

       do k=1, oi%sound(n)%info%levels

         CALL Stats_Calculate( n, k, oi%Sound(n)%u(k)%qc, &
                               oi%Sound(n)%u(k)%inv, nu, &
                               Sound%minimum%u, Sound%maximum%u, &
                               Sound%average%u, Sound%rms_err%u)
         CALL Stats_Calculate( n, k, oi%Sound(n)%v(k)%qc, &
                               oi%Sound(n)%v(k)%inv, nv, &
                               Sound%minimum%v, Sound%maximum%v, &
                               Sound%average%v, Sound%rms_err%v)
         CALL Stats_Calculate( n, k, oi%Sound(n)%t(k)%qc, &
                               oi%Sound(n)%t(k)%inv, nt, &
                               Sound%minimum%t, Sound%maximum%t, &
                               Sound%average%t, Sound%rms_err%t)
         CALL Stats_Calculate( n, k, oi%Sound(n)%q(k)%qc, &
                               oi%Sound(n)%q(k)%inv, nq, &
                               Sound%minimum%q, Sound%maximum%q, &
                               Sound%average%q, Sound%rms_err%q)
       end do
      endif    ! end if( oi%sound(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( nq )
   
   call proc_stats_combine(Sound%average%u, Sound%rms_err%u, &
                           Sound%minimum%u%value, Sound%maximum%u%value, &
                           Sound%minimum%u%n, Sound%maximum%u%n, &
                           Sound%minimum%u%l, Sound%maximum%u%l )
   call proc_stats_combine(Sound%average%v, Sound%rms_err%v, &
                           Sound%minimum%v%value, Sound%maximum%v%value, &
                           Sound%minimum%v%n, Sound%maximum%v%n, &
                           Sound%minimum%v%l, Sound%maximum%v%l )
   call proc_stats_combine(Sound%average%t, Sound%rms_err%t, &
                           Sound%minimum%t%value, Sound%maximum%t%value, &
                           Sound%minimum%t%n, Sound%maximum%t%n, &
                           Sound%minimum%t%l, Sound%maximum%t%l )
   call proc_stats_combine(Sound%average%q, Sound%rms_err%q, &
                           Sound%minimum%q%value, Sound%maximum%q%value, &
                           Sound%minimum%q%n, Sound%maximum%q%n, &
                           Sound%minimum%q%l, Sound%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
     IF (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. nq /= 0) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Sound'
   
      CALL DA_Print_Stats_Sound( stats_unit, nu, nv, nt, nq, Sound )
   
     END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Sound

