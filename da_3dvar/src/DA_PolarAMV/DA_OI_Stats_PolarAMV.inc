SUBROUTINE DA_OI_Stats_PolarAMV( stats_unit, oi )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: Calculates (Obs - Analysis) statistics for Polar AMVs
!               
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_polaramv_type)       :: polaramv
   INTEGER                          :: nu, nv, nt
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   
   polaramv%maximum%u = maxmin_type( missing_r, 0, 0)
   polaramv%maximum%v = maxmin_type( missing_r, 0, 0)
   polaramv%minimum%u = maxmin_type(-missing_r, 0, 0)
   polaramv%minimum%v = maxmin_type(-missing_r, 0, 0)
   polaramv%average = residual_polaramv1_type(0.0, 0.0)
   polaramv%rms_err = polaramv%average

   IF ( oi%num_polaramv > 0 ) THEN

      DO n=1, oi%num_polaramv

       if( oi%polaramv(n)%loc%proc_domain ) then
       do k=1, oi%polaramv(n)%info%levels

         CALL Stats_Calculate( n, 0, oi%polaramv(n)%u(k)%qc, &
                               oi%polaramv(n)%u(k)%inv, nu, &
                               polaramv%minimum%u, polaramv%maximum%u, &
                               polaramv%average%u, polaramv%rms_err%u )

         CALL Stats_Calculate( n, 0, oi%polaramv(n)%v(k)%qc, &
                               oi%polaramv(n)%v(k)%inv, nv, &
                               polaramv%minimum%v, polaramv%maximum%v, &
                               polaramv%average%v, polaramv%rms_err%v )

       end do

       endif    ! end if( oi%polaramv(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   call proc_stats_combine(polaramv%average%u, polaramv%rms_err%u, &
                           polaramv%minimum%u%value, polaramv%maximum%u%value, &
                           polaramv%minimum%u%n, polaramv%maximum%u%n, &
                           polaramv%minimum%u%l, polaramv%maximum%u%l )
   call proc_stats_combine(polaramv%average%v, polaramv%rms_err%v, &
                           polaramv%minimum%v%value, polaramv%maximum%v%value, &
                           polaramv%minimum%v%n, polaramv%maximum%v%n, &
                           polaramv%minimum%v%l, polaramv%maximum%v%l )
   
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF (nu /= 0 .or. nv /= 0 ) THEN
   
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for polaramv'
   
      CALL DA_Print_Stats_PolarAMV( stats_unit, nu, nv, polaramv )
   
    END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_PolarAMV
