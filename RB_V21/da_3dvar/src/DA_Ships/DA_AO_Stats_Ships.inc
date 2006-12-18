SUBROUTINE DA_AO_Stats_Ships( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_ships_type)          :: ships
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   num_ships_tot = 0

   ships%maximum%u = maxmin_type( missing_r, 0, 0)
   ships%maximum%v = maxmin_type( missing_r, 0, 0)
   ships%maximum%t = maxmin_type( missing_r, 0, 0)
   ships%maximum%p = maxmin_type( missing_r, 0, 0)
   ships%maximum%q = maxmin_type( missing_r, 0, 0)
   ships%minimum%u = maxmin_type(-missing_r, 0, 0)
   ships%minimum%v = maxmin_type(-missing_r, 0, 0)
   ships%minimum%t = maxmin_type(-missing_r, 0, 0)
   ships%minimum%p = maxmin_type(-missing_r, 0, 0)
   ships%minimum%q = maxmin_type(-missing_r, 0, 0)
   ships%average = residual_ships1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   ships%rms_err = ships%average

   IF ( oi%num_ships > 0 ) THEN

      DO n=1, oi%num_ships

       if( oi%ships(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%ships(n)%u%qc, & 
                               re%ships(n)%u, nu, & 
                               ships%minimum%u, ships%maximum%u, &
                               ships%average%u, ships%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%ships(n)%v%qc, & 
                               re%ships(n)%v, nv, & 
                               ships%minimum%v, ships%maximum%v, &
                               ships%average%v, ships%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%ships(n)%t%qc, & 
                               re%ships(n)%t, nt, & 
                               ships%minimum%t, ships%maximum%t, &
                               ships%average%t, ships%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%ships(n)%p%qc, & 
                               re%ships(n)%p, np, & 
                               ships%minimum%p, ships%maximum%p, &
                               ships%average%p, ships%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%ships(n)%q%qc, & 
                               re%ships(n)%q, nq, & 
                               ships%minimum%q, ships%maximum%q, &
                               ships%average%q, ships%rms_err%q )

       endif    ! end if( oi%ships(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   num_ships_tot = nu + nv + nt + np + nq
   
   call proc_stats_combine(Ships%average%u, Ships%rms_err%u, &
                           Ships%minimum%u%value, Ships%maximum%u%value, &
                           Ships%minimum%u%n, Ships%maximum%u%n, &
                           Ships%minimum%u%l, Ships%maximum%u%l )
   call proc_stats_combine(Ships%average%v, Ships%rms_err%v, &
                           Ships%minimum%v%value, Ships%maximum%v%value, &
                           Ships%minimum%v%n, Ships%maximum%v%n, &
                           Ships%minimum%v%l, Ships%maximum%v%l )
   call proc_stats_combine(Ships%average%t, Ships%rms_err%t, &
                           Ships%minimum%t%value, Ships%maximum%t%value, &
                           Ships%minimum%t%n, Ships%maximum%t%n, &
                           Ships%minimum%t%l, Ships%maximum%t%l )
   call proc_stats_combine(Ships%average%p, Ships%rms_err%p, &
                           Ships%minimum%p%value, Ships%maximum%p%value, &
                           Ships%minimum%p%n, Ships%maximum%p%n, &
                           Ships%minimum%p%l, Ships%maximum%p%l )
   call proc_stats_combine(Ships%average%q, Ships%rms_err%q, &
                           Ships%minimum%q%value, Ships%maximum%q%value, &
                           Ships%minimum%q%n, Ships%maximum%q%n, &
                           Ships%minimum%q%l, Ships%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ships'
   
      call DA_Print_Stats_Ships(stats_unit, nu, nv, nt, np, nq, ships)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Ships
