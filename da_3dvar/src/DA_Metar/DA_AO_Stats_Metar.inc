SUBROUTINE DA_AO_Stats_Metar( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_metar_type)                :: metar
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   num_metar_tot = 0

   metar%maximum%u = maxmin_type( missing_r, 0, 0)
   metar%maximum%v = maxmin_type( missing_r, 0, 0)
   metar%maximum%t = maxmin_type( missing_r, 0, 0)
   metar%maximum%p = maxmin_type( missing_r, 0, 0)
   metar%maximum%q = maxmin_type( missing_r, 0, 0)
   metar%minimum%u = maxmin_type(-missing_r, 0, 0)
   metar%minimum%v = maxmin_type(-missing_r, 0, 0)
   metar%minimum%t = maxmin_type(-missing_r, 0, 0)
   metar%minimum%p = maxmin_type(-missing_r, 0, 0)
   metar%minimum%q = maxmin_type(-missing_r, 0, 0)
   metar%average = residual_metar1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   metar%rms_err = metar%average

   IF ( oi%num_metar > 0 ) THEN

      DO n=1, oi%num_metar

       if( oi%metar(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%metar(n)%u%qc, & 
                               re%metar(n)%u, nu, &
                               metar%minimum%u, metar%maximum%u, &
                               metar%average%u, metar%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%metar(n)%v%qc, & 
                               re%metar(n)%v, nv, &
                               metar%minimum%v, metar%maximum%v, &
                               metar%average%v, metar%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%metar(n)%t%qc, & 
                               re%metar(n)%t, nt, &
                               metar%minimum%t, metar%maximum%t, &
                               metar%average%t, metar%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%metar(n)%p%qc, & 
                               re%metar(n)%p, np, &
                               metar%minimum%p, metar%maximum%p, &
                               metar%average%p, metar%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%metar(n)%q%qc, & 
                               re%metar(n)%q, nq, &
                               metar%minimum%q, metar%maximum%q, &
                               metar%average%q, metar%rms_err%q )

       endif    ! end if( oi%metar(n)%loc%proc_domain )

      END DO
      
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   num_metar_tot = nu + nv + nt + np + nq
   
   call proc_stats_combine(Metar%average%u, Metar%rms_err%u, &
                           Metar%minimum%u%value, Metar%maximum%u%value, &
                           Metar%minimum%u%n, Metar%maximum%u%n, &
                           Metar%minimum%u%l, Metar%maximum%u%l )
   call proc_stats_combine(Metar%average%v, Metar%rms_err%v, &
                           Metar%minimum%v%value, Metar%maximum%v%value, &
                           Metar%minimum%v%n, Metar%maximum%v%n, &
                           Metar%minimum%v%l, Metar%maximum%v%l )
   call proc_stats_combine(Metar%average%t, Metar%rms_err%t, &
                           Metar%minimum%t%value, Metar%maximum%t%value, &
                           Metar%minimum%t%n, Metar%maximum%t%n, &
                           Metar%minimum%t%l, Metar%maximum%t%l )
   call proc_stats_combine(Metar%average%p, Metar%rms_err%p, &
                           Metar%minimum%p%value, Metar%maximum%p%value, &
                           Metar%minimum%p%n, Metar%maximum%p%n, &
                           Metar%minimum%p%l, Metar%maximum%p%l )
   call proc_stats_combine(Metar%average%q, Metar%rms_err%q, &
                           Metar%minimum%q%value, Metar%maximum%q%value, &
                           Metar%minimum%q%n, Metar%maximum%q%n, &
                           Metar%minimum%q%l, Metar%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for metar'
   
      call DA_Print_Stats_Metar(stats_unit, nu, nv, nt, np, nq, metar)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Metar

