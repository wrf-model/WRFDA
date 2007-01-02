SUBROUTINE DA_OI_Stats_Buoy ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_buoy_type)           :: buoy
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   
   buoy%maximum%u = maxmin_type( missing_r, 0, 0)
   buoy%maximum%v = maxmin_type( missing_r, 0, 0)
   buoy%maximum%t = maxmin_type( missing_r, 0, 0)
   buoy%maximum%p = maxmin_type( missing_r, 0, 0)
   buoy%maximum%q = maxmin_type( missing_r, 0, 0)
   buoy%minimum%u = maxmin_type(-missing_r, 0, 0)
   buoy%minimum%v = maxmin_type(-missing_r, 0, 0)
   buoy%minimum%t = maxmin_type(-missing_r, 0, 0)
   buoy%minimum%p = maxmin_type(-missing_r, 0, 0)
   buoy%minimum%q = maxmin_type(-missing_r, 0, 0)
   buoy%average = residual_buoy1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   buoy%rms_err = buoy%average

   IF ( oi%num_buoy > 0 ) THEN

      DO n=1, oi%num_buoy

       if( oi%buoy(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%buoy(n)%u%qc, &
                               oi%buoy(n)%u%inv, nu, &
                               buoy%minimum%u, buoy%maximum%u, &
                               buoy%average%u, buoy%rms_err%u)
         CALL Stats_Calculate( n, 0, oi%buoy(n)%v%qc, &
                               oi%buoy(n)%v%inv, nv, &
                               buoy%minimum%v, buoy%maximum%v, &
                               buoy%average%v, buoy%rms_err%v)
         CALL Stats_Calculate( n, 0, oi%buoy(n)%t%qc, &
                               oi%buoy(n)%t%inv, nt, &
                               buoy%minimum%t, buoy%maximum%t, &
                               buoy%average%t, buoy%rms_err%t)
         CALL Stats_Calculate( n, 0, oi%buoy(n)%p%qc, &
                               oi%buoy(n)%p%inv, np, &
                               buoy%minimum%p, buoy%maximum%p, &
                               buoy%average%p, buoy%rms_err%p)
         CALL Stats_Calculate( n, 0, oi%buoy(n)%q%qc, &
                               oi%buoy(n)%q%inv, nq, &
                               buoy%minimum%q, buoy%maximum%q, &
                               buoy%average%q, buoy%rms_err%q)

       endif    ! end if( oi%buoy(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   
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
   
    IF (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for buoy'
   
      CALL DA_Print_Stats_Buoy( stats_unit, nu, nv, nt, np, nq, buoy )
   
    END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Buoy
