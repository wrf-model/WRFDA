subroutine da_ob_stats_qscat ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_qscat_type)          :: qscat
   INTEGER                          :: nu, nv
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   
   qscat%maximum%u = maxmin_type(-1.0E+20, 0, 0)
   qscat%maximum%v = maxmin_type(-1.0E+20, 0, 0)
   qscat%minimum%u = maxmin_type( 1.0E+20, 0, 0)
   qscat%minimum%v = maxmin_type( 1.0E+20, 0, 0)
   qscat%average = residual_qscat1_type(0.0, 0.0)
   qscat%rms_err = qscat%average

   IF ( oi%num_qscat > 0 ) THEN

      DO n=1, oi%num_qscat

       if( oi%qscat(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%qscat(n)%u%qc, &
                               oi%qscat(n)%u%inv, nu, &
                               qscat%minimum%u, qscat%maximum%u, &
                               qscat%average%u, qscat%rms_err%u)
         CALL Stats_Calculate( n, 0, oi%qscat(n)%v%qc, &
                               oi%qscat(n)%v%inv, nv, &
                               qscat%minimum%v, qscat%maximum%v, &
                               qscat%average%v, qscat%rms_err%v)

       endif    ! end if( oi%qscat(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   
   call proc_stats_combine(qscat%average%u, qscat%rms_err%u, &
                           qscat%minimum%u%value, qscat%maximum%u%value, &
                           qscat%minimum%u%n, qscat%maximum%u%n, &
                           qscat%minimum%u%l, qscat%maximum%u%l )
   call proc_stats_combine(qscat%average%v, qscat%rms_err%v, &
                           qscat%minimum%v%value, qscat%maximum%v%value, &
                           qscat%minimum%v%n, qscat%maximum%v%n, &
                           qscat%minimum%v%l, qscat%maximum%v%l )
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF (nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for qscat'
   
      CALL DA_Print_Stats_qscat( stats_unit, nu, nv, qscat )
   
    END IF
   
   END IF

end subroutine da_ob_stats_qscat

