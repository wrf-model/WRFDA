SUBROUTINE DA_OI_Stats_Satob ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_type)                :: satob
   INTEGER                          :: nu, nv, nt
   INTEGER                          :: n

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   
   satob%maximum%u = maxmin_type( missing_r, 0, 0)
   satob%maximum%v = maxmin_type( missing_r, 0, 0)
   satob%minimum%u = maxmin_type(-missing_r, 0, 0)
   satob%minimum%v = maxmin_type(-missing_r, 0, 0)
   satob%average = residual_type(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
   satob%rms_err = satob%average

   IF ( oi%num_satob > 0 ) THEN

      DO n=1, oi%num_satob

       if( oi%satob(n)%loc%proc_domain ) then

         CALL Stats_Calculate( n, 0, oi%satob(n)%u%qc, &
                               oi%satob(n)%u%inv, nu, &
                               satob%minimum%u, satob%maximum%u, &
                               satob%average%u, satob%rms_err%u )

         CALL Stats_Calculate( n, 0, oi%satob(n)%v%qc, &
                               oi%satob(n)%v%inv, nv, &
                               satob%minimum%v, satob%maximum%v, &
                               satob%average%v, satob%rms_err%v )

       endif    ! end if( oi%satob(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_stats_combine(Satob%average%u, Satob%rms_err%u, &
                           Satob%minimum%u%value, Satob%maximum%u%value, &
                           Satob%minimum%u%n, Satob%maximum%u%n, &
                           Satob%minimum%u%l, Satob%maximum%u%l )
   call proc_stats_combine(Satob%average%v, Satob%rms_err%v, &
                           Satob%minimum%v%value, Satob%maximum%v%value, &
                           Satob%minimum%v%n, Satob%maximum%v%n, &
                           Satob%minimum%v%l, Satob%maximum%v%l )
   
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF (nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for satob'
   
      CALL DA_Print_Stats_Satob( stats_unit, nu, nv, satob )
   
    END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Satob

