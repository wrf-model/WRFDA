SUBROUTINE DA_AO_Stats_Buoy( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_buoy_type)           :: Buoy
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   num_Buoy_tot = 0

   Buoy%maximum%u = maxmin_type( missing_r, 0, 0)
   Buoy%maximum%v = maxmin_type( missing_r, 0, 0)
   Buoy%maximum%t = maxmin_type( missing_r, 0, 0)
   Buoy%maximum%p = maxmin_type( missing_r, 0, 0)
   Buoy%maximum%q = maxmin_type( missing_r, 0, 0)
   Buoy%minimum%u = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%v = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%t = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%p = maxmin_type(-missing_r, 0, 0)
   Buoy%minimum%q = maxmin_type(-missing_r, 0, 0)
   Buoy%average = residual_buoy1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   Buoy%rms_err = Buoy%average

   IF ( oi%num_Buoy > 0 ) THEN

      DO n=1, oi%num_Buoy

       if( oi%Buoy(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%Buoy(n)%u%qc, & 
                               re%Buoy(n)%u, nu, & 
                               Buoy%minimum%u, Buoy%maximum%u, &
                               Buoy%average%u, Buoy%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%Buoy(n)%v%qc, & 
                               re%Buoy(n)%v, nv, & 
                               Buoy%minimum%v, Buoy%maximum%v, &
                               Buoy%average%v, Buoy%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%Buoy(n)%t%qc, & 
                               re%Buoy(n)%t, nt, & 
                               Buoy%minimum%t, Buoy%maximum%t, &
                               Buoy%average%t, Buoy%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%Buoy(n)%p%qc, & 
                               re%Buoy(n)%p, np, & 
                               Buoy%minimum%p, Buoy%maximum%p, &
                               Buoy%average%p, Buoy%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%Buoy(n)%q%qc, & 
                               re%Buoy(n)%q, nq, & 
                               Buoy%minimum%q, Buoy%maximum%q, &
                               Buoy%average%q, Buoy%rms_err%q )

       endif    ! end if( oi%Buoy(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   num_Buoy_tot = nu + nv + nt + np + nq
   
   call proc_stats_combine(Buoy%average%u, Buoy%rms_err%u, &
                           Buoy%minimum%u%value, Buoy%maximum%u%value, &
                           Buoy%minimum%u%n, Buoy%maximum%u%n, &
                           Buoy%minimum%u%l, Buoy%maximum%u%l )
   call proc_stats_combine(Buoy%average%v, Buoy%rms_err%v, &
                           Buoy%minimum%v%value, Buoy%maximum%v%value, &
                           Buoy%minimum%v%n, Buoy%maximum%v%n, &
                           Buoy%minimum%v%l, Buoy%maximum%v%l )
   call proc_stats_combine(Buoy%average%t, Buoy%rms_err%t, &
                           Buoy%minimum%t%value, Buoy%maximum%t%value, &
                           Buoy%minimum%t%n, Buoy%maximum%t%n, &
                           Buoy%minimum%t%l, Buoy%maximum%t%l )
   call proc_stats_combine(Buoy%average%p, Buoy%rms_err%p, &
                           Buoy%minimum%p%value, Buoy%maximum%p%value, &
                           Buoy%minimum%p%n, Buoy%maximum%p%n, &
                           Buoy%minimum%p%l, Buoy%maximum%p%l )
   call proc_stats_combine(Buoy%average%q, Buoy%rms_err%q, &
                           Buoy%minimum%q%value, Buoy%maximum%q%value, &
                           Buoy%minimum%q%n, Buoy%maximum%q%n, &
                           Buoy%minimum%q%l, Buoy%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Buoy'
   
      call DA_Print_Stats_Buoy(stats_unit, nu, nv, nt, np, nq, Buoy)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Buoy
