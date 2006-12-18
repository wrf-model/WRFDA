SUBROUTINE DA_AO_Stats_Pilot( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_pilot_type)          :: Pilot
   INTEGER                          :: nu, nv
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

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
         CALL Stats_Calculate( n, k, oi%Pilot(n)%u(k)%qc, & 
                               re%Pilot(n)%u(k), nu, & 
                               Pilot%minimum%u, Pilot%maximum%u, &
                               Pilot%average%u, Pilot%rms_err%u )
         CALL Stats_Calculate( n, k, oi%Pilot(n)%v(k)%qc, & 
                               re%Pilot(n)%v(k), nv, & 
                               Pilot%minimum%v, Pilot%maximum%v, &
                               Pilot%average%v, Pilot%rms_err%v )
       end do
      endif    ! end if( oi%pilot(n)%loc%proc_domain )
      END DO
      
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   num_pilot_tot = nu + nv
   
   call proc_stats_combine(Pilot%average%u, Pilot%rms_err%u, &
                           Pilot%minimum%u%value, Pilot%maximum%u%value, &
                           Pilot%minimum%u%n, Pilot%maximum%u%n, &
                           Pilot%minimum%u%l, Pilot%maximum%u%l )
   call proc_stats_combine(Pilot%average%v, Pilot%rms_err%v, &
                           Pilot%minimum%v%value, Pilot%maximum%v%value, &
                           Pilot%minimum%v%n, Pilot%maximum%v%n, &
                           Pilot%minimum%v%l, Pilot%maximum%v%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Pilot'
   
      call DA_Print_Stats_Pilot(stats_unit, nu, nv, Pilot)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Pilot

