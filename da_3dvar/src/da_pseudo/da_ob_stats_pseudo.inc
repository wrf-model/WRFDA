SUBROUTINE da_ob_stats_pseudo ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! O-B

   TYPE (stats_pseudo_type)         :: pseudo
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   
   pseudo%maximum%u = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%v = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%t = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%p = maxmin_type( missing_r, 0, 0)
   pseudo%maximum%q = maxmin_type( missing_r, 0, 0)
   pseudo%minimum%u = maxmin_type( -missing_r, 0, 0)
   pseudo%minimum%v = maxmin_type( -missing_r, 0, 0)
   pseudo%minimum%t = maxmin_type( -missing_r, 0, 0)
   pseudo%minimum%p = maxmin_type( -missing_r, 0, 0)
   pseudo%minimum%q = maxmin_type( -missing_r, 0, 0)
   pseudo%average = residual_pseudo1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   pseudo%rms_err = pseudo%average
   
   IF ( oi%num_pseudo > 0 ) THEN

      DO n=1, oi%num_pseudo

       if( oi%pseudo(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%pseudo(n)%u%qc, & 
                               oi%pseudo(n)%u%inv, nu, & 
                               pseudo%minimum%u, pseudo%maximum%u, &
                               pseudo%average%u, pseudo%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%v%qc, & 
                               oi%pseudo(n)%v%inv, nv, & 
                               pseudo%minimum%v, pseudo%maximum%v, &
                               pseudo%average%v, pseudo%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%t%qc, & 
                               oi%pseudo(n)%t%inv, nt, & 
                               pseudo%minimum%t, pseudo%maximum%t, &
                               pseudo%average%t, pseudo%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%p%qc, & 
                               oi%pseudo(n)%p%inv, np, & 
                               pseudo%minimum%p, pseudo%maximum%p, &
                               pseudo%average%p, pseudo%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%pseudo(n)%q%qc, & 
                               oi%pseudo(n)%q%inv, nq, & 
                               pseudo%minimum%q, pseudo%maximum%q, &
                               pseudo%average%q, pseudo%rms_err%q )

       endif    ! end if( oi%pseudo(n)%loc%proc_domain )

      END DO
      
   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   
   call proc_stats_combine(Pseudo%average%u, Pseudo%rms_err%u, &
                           Pseudo%minimum%u%value, Pseudo%maximum%u%value, &
                           Pseudo%minimum%u%n, Pseudo%maximum%u%n, &
                           Pseudo%minimum%u%l, Pseudo%maximum%u%l )
   call proc_stats_combine(Pseudo%average%v, Pseudo%rms_err%v, &
                           Pseudo%minimum%v%value, Pseudo%maximum%v%value, &
                           Pseudo%minimum%v%n, Pseudo%maximum%v%n, &
                           Pseudo%minimum%v%l, Pseudo%maximum%v%l )
   call proc_stats_combine(Pseudo%average%t, Pseudo%rms_err%t, &
                           Pseudo%minimum%t%value, Pseudo%maximum%t%value, &
                           Pseudo%minimum%t%n, Pseudo%maximum%t%n, &
                           Pseudo%minimum%t%l, Pseudo%maximum%t%l )
   call proc_stats_combine(Pseudo%average%p, Pseudo%rms_err%p, &
                           Pseudo%minimum%p%value, Pseudo%maximum%p%value, &
                           Pseudo%minimum%p%n, Pseudo%maximum%p%n, &
                           Pseudo%minimum%p%l, Pseudo%maximum%p%l )
   call proc_stats_combine(Pseudo%average%q, Pseudo%rms_err%q, &
                           Pseudo%minimum%q%value, Pseudo%maximum%q%value, &
                           Pseudo%minimum%q%n, Pseudo%maximum%q%n, &
                           Pseudo%minimum%q%l, Pseudo%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .OR. nt /= 0 .or. np /= 0 .OR. nq /= 0 ) THEN
   
     write(unit=stats_unit, fmt='(/a/)') ' O-B Diagnostics for pseudo'
   
     CALL da_print_stats_pseudo( stats_unit, nu, nv, nt, np, nq, pseudo )
   
    END IF
   
   END IF

END SUBROUTINE da_ob_stats_pseudo
