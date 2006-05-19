SUBROUTINE DA_OI_Stats_Bogus ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_bogus_type)          :: Bogus
   INTEGER                          :: nu, nv, nt, nq, nslp
   INTEGER                          :: n, k
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nslp = 0
   nu = 0
   nv = 0
   nt = 0
   nq = 0

   Bogus%maximum%u = maxmin_type( missing_r, 0, 0)
   Bogus%maximum%v = maxmin_type( missing_r, 0, 0)
   Bogus%maximum%t = maxmin_type( missing_r, 0, 0)
   Bogus%maximum%q = maxmin_type( missing_r, 0, 0)
   Bogus%maximum%slp = maxmin_type( missing_r, 0, 0)
   Bogus%minimum%u = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%v = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%t = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%q = maxmin_type(-missing_r, 0, 0)
   Bogus%minimum%slp = maxmin_type(-missing_r, 0, 0)
   Bogus%average = residual_bogus1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   Bogus%rms_err = Bogus%average

   IF ( oi%num_Bogus > 0 ) THEN
     
      DO n=1, oi%num_Bogus

      if( oi%bogus(n)%loc%proc_domain ) then

      do k=1, oi%bogus(n)%info%levels
         CALL Stats_Calculate( n, k, oi%Bogus(n)%u(k)%qc, &
                               oi%Bogus(n)%u(k)%inv, nu, &
                               Bogus%minimum%u, Bogus%maximum%u, &
                               Bogus%average%u, Bogus%rms_err%u)
         CALL Stats_Calculate( n, k, oi%Bogus(n)%v(k)%qc, &
                               oi%Bogus(n)%v(k)%inv, nv, &
                               Bogus%minimum%v, Bogus%maximum%v, &
                               Bogus%average%v, Bogus%rms_err%v)
         CALL Stats_Calculate( n, k, oi%Bogus(n)%t(k)%qc, &
                               oi%Bogus(n)%t(k)%inv, nt, &
                               Bogus%minimum%t, Bogus%maximum%t, &
                               Bogus%average%t, Bogus%rms_err%t)
         CALL Stats_Calculate( n, k, oi%Bogus(n)%q(k)%qc, &
                               oi%Bogus(n)%q(k)%inv, nq, &
                               Bogus%minimum%q, Bogus%maximum%q, &
                               Bogus%average%q, Bogus%rms_err%q)
      end do
      CALL Stats_Calculate( n, 0, oi%Bogus(n)%slp%qc, &
                            oi%Bogus(n)%slp%inv, nslp,  &
                            Bogus%minimum%slp, Bogus%maximum%slp, &
                            Bogus%average%slp, Bogus%rms_err%slp)
      end if
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( nq )
   call proc_sum_int( nslp )

   call proc_stats_combine(Bogus%average%u, Bogus%rms_err%u, &
                           Bogus%minimum%u%value, Bogus%maximum%u%value, &
                           Bogus%minimum%u%n, Bogus%maximum%u%n, &
                           Bogus%minimum%u%l, Bogus%maximum%u%l )
   call proc_stats_combine(Bogus%average%v, Bogus%rms_err%v, &
                           Bogus%minimum%v%value, Bogus%maximum%v%value, &
                           Bogus%minimum%v%n, Bogus%maximum%v%n, &
                           Bogus%minimum%v%l, Bogus%maximum%v%l )
   call proc_stats_combine(Bogus%average%t, Bogus%rms_err%t, &
                           Bogus%minimum%t%value, Bogus%maximum%t%value, &
                           Bogus%minimum%t%n, Bogus%maximum%t%n, &
                           Bogus%minimum%t%l, Bogus%maximum%t%l )
   call proc_stats_combine(Bogus%average%q, Bogus%rms_err%q, &
                           Bogus%minimum%q%value, Bogus%maximum%q%value, &
                           Bogus%minimum%q%n, Bogus%maximum%q%n, &
                           Bogus%minimum%q%l, Bogus%maximum%q%l )
   call proc_stats_combine(Bogus%average%slp, Bogus%rms_err%slp, &
                           Bogus%minimum%slp%value, Bogus%maximum%slp%value, &
                           Bogus%minimum%slp%n, Bogus%maximum%slp%n, &
                           Bogus%minimum%slp%l, Bogus%maximum%slp%l )

   IF ( wrf_dm_on_monitor() ) THEN


      IF (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. nq /= 0 .or. nslp /= 0 ) THEN

         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Bogus'

         CALL DA_Print_Stats_Bogus( stats_unit, nu, nv, nt, nq, nslp, Bogus )

      END IF

   END IF

END SUBROUTINE DA_OI_Stats_Bogus
