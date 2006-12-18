SUBROUTINE DA_AO_Stats_Sonde_sfc( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_sonde_sfc_type)      :: sonde_sfc
   INTEGER                          :: nu, nv, nt, np, nq
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0
   
   sonde_sfc%maximum%u = maxmin_type( missing_r, 0, 0)
   sonde_sfc%maximum%v = maxmin_type( missing_r, 0, 0)
   sonde_sfc%maximum%t = maxmin_type( missing_r, 0, 0)
   sonde_sfc%maximum%p = maxmin_type( missing_r, 0, 0)
   sonde_sfc%maximum%q = maxmin_type( missing_r, 0, 0)
   sonde_sfc%minimum%u = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%v = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%t = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%p = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%minimum%q = maxmin_type(-missing_r, 0, 0)
   sonde_sfc%average = residual_sonde_sfc1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   sonde_sfc%rms_err = sonde_sfc%average

   IF ( oi%num_sound > 0 ) THEN

      DO n=1, oi%num_sound

       if( oi%sonde_sfc(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%sonde_sfc(n)%u%qc,  & 
                               re%sonde_sfc(n)%u, nu, & 
                               sonde_sfc%minimum%u, sonde_sfc%maximum%u, &
                               sonde_sfc%average%u, sonde_sfc%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%sonde_sfc(n)%v%qc,  & 
                               re%sonde_sfc(n)%v, nv, & 
                               sonde_sfc%minimum%v, sonde_sfc%maximum%v, &
                               sonde_sfc%average%v, sonde_sfc%rms_err%v )
         CALL Stats_Calculate( n, 0, oi%sonde_sfc(n)%t%qc,  & 
                               re%sonde_sfc(n)%t, nt, & 
                               sonde_sfc%minimum%t, sonde_sfc%maximum%t, &
                               sonde_sfc%average%t, sonde_sfc%rms_err%t )
         CALL Stats_Calculate( n, 0, oi%sonde_sfc(n)%p%qc,  & 
                               re%sonde_sfc(n)%p, np, & 
                               sonde_sfc%minimum%p, sonde_sfc%maximum%p, &
                               sonde_sfc%average%p, sonde_sfc%rms_err%p )
         CALL Stats_Calculate( n, 0, oi%sonde_sfc(n)%q%qc,  & 
                               re%sonde_sfc(n)%q, nq, & 
                               sonde_sfc%minimum%q, sonde_sfc%maximum%q, &
                               sonde_sfc%average%q, sonde_sfc%rms_err%q )

       endif    ! end if( oi%sonde_sfc(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_sum_int( nt )
   call proc_sum_int( np )
   call proc_sum_int( nq )
   num_sound_tot = num_sound_tot + nu + nv + nt + np + nq
   
   call proc_stats_combine(Sonde_sfc%average%u, Sonde_sfc%rms_err%u, &
                           Sonde_sfc%minimum%u%value, Sonde_sfc%maximum%u%value, &
                           Sonde_sfc%minimum%u%n, Sonde_sfc%maximum%u%n, &
                           Sonde_sfc%minimum%u%l, Sonde_sfc%maximum%u%l )
   call proc_stats_combine(Sonde_sfc%average%v, Sonde_sfc%rms_err%v, &
                           Sonde_sfc%minimum%v%value, Sonde_sfc%maximum%v%value, &
                           Sonde_sfc%minimum%v%n, Sonde_sfc%maximum%v%n, &
                           Sonde_sfc%minimum%v%l, Sonde_sfc%maximum%v%l )
   call proc_stats_combine(Sonde_sfc%average%t, Sonde_sfc%rms_err%t, &
                           Sonde_sfc%minimum%t%value, Sonde_sfc%maximum%t%value, &
                           Sonde_sfc%minimum%t%n, Sonde_sfc%maximum%t%n, &
                           Sonde_sfc%minimum%t%l, Sonde_sfc%maximum%t%l )
   call proc_stats_combine(Sonde_sfc%average%p, Sonde_sfc%rms_err%p, &
                           Sonde_sfc%minimum%p%value, Sonde_sfc%maximum%p%value, &
                           Sonde_sfc%minimum%p%n, Sonde_sfc%maximum%p%n, &
                           Sonde_sfc%minimum%p%l, Sonde_sfc%maximum%p%l )
   call proc_stats_combine(Sonde_sfc%average%q, Sonde_sfc%rms_err%q, &
                           Sonde_sfc%minimum%q%value, Sonde_sfc%maximum%q%value, &
                           Sonde_sfc%minimum%q%n, Sonde_sfc%maximum%q%n, &
                           Sonde_sfc%minimum%q%l, Sonde_sfc%maximum%q%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for sonde_sfc'
   
      call DA_Print_Stats_Sonde_sfc(stats_unit, nu, nv, nt, np, nq, sonde_sfc)
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_Sonde_sfc





