SUBROUTINE DA_OI_Stats_Airep ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_airep_type)                :: Airep
   INTEGER                          :: nu, nv, nt
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

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
         CALL Stats_Calculate( n, k, oi%Airep(n)%u(k)%qc, &
                               oi%Airep(n)%u(k)%inv, nu, &
                               Airep%minimum%u, Airep%maximum%u, &
                               Airep%average%u, Airep%rms_err%u)
         CALL Stats_Calculate( n, k, oi%Airep(n)%v(k)%qc, &
                               oi%Airep(n)%v(k)%inv, nv, &
                               Airep%minimum%v, Airep%maximum%v, &
                               Airep%average%v, Airep%rms_err%v)
         CALL Stats_Calculate( n, k, oi%Airep(n)%t(k)%qc, &
                               oi%Airep(n)%t(k)%inv, nt, &
                               Airep%minimum%t, Airep%maximum%t, &
                               Airep%average%t, Airep%rms_err%t)
       end do

      endif    ! end if( oi%airep(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   
   call proc_stats_combine(Airep%average%u, Airep%rms_err%u, &
                           Airep%minimum%u%value, Airep%maximum%u%value, &
                           Airep%minimum%u%n, Airep%maximum%u%n, &
                           Airep%minimum%u%l, Airep%maximum%u%l )
   call proc_stats_combine(Airep%average%v, Airep%rms_err%v, &
                           Airep%minimum%v%value, Airep%maximum%v%value, &
                           Airep%minimum%v%n, Airep%maximum%v%n, &
                           Airep%minimum%v%l, Airep%maximum%v%l )
   call proc_stats_combine(Airep%average%t, Airep%rms_err%t, &
                           Airep%minimum%t%value, Airep%maximum%t%value, &
                           Airep%minimum%t%n, Airep%maximum%t%n, &
                           Airep%minimum%t%l, Airep%maximum%t%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF (nu /= 0 .or. nv /= 0 .or. nt /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Airep'
   
      CALL DA_Print_Stats_Airep( stats_unit, nu, nv, nt, Airep )
   
    END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Airep
