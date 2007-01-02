SUBROUTINE DA_AO_Stats_GeoAMV( stats_unit, oi, re )
!-------------------------------------------------------------------------
!  HISTORY:
!
!      Created: 03/05/2005                   Syed RH Rizvi
!
!      Purpose: Calculates (Obs - Analysis) statistics for Geo.  AMV's
!
!-------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_geoamv_type)         :: geoamv
   INTEGER                          :: nu, nv
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nu = 0
   nv = 0
   num_geoamv_tot = 0
   
   geoamv%maximum%u = maxmin_type( missing_r, 0, 0)
   geoamv%maximum%v = maxmin_type( missing_r, 0, 0)
   geoamv%minimum%u = maxmin_type(-missing_r, 0, 0)
   geoamv%minimum%v = maxmin_type(-missing_r, 0, 0)
   geoamv%average = residual_geoamv1_type( 0.0, 0.0)
   geoamv%rms_err = geoamv%average

   IF ( oi%num_geoamv > 0 ) THEN
   
      DO n=1, oi%num_geoamv

       if( oi%geoamv(n)%loc%proc_domain ) then

       DO k=1, oi%geoamv(n)%info%levels
         CALL Stats_Calculate( n, 0, oi%geoamv(n)%u(k)%qc, & 
                               re%geoamv(n)%u(k), nu, & 
                               geoamv%minimum%u, geoamv%maximum%u, &
                               geoamv%average%u, geoamv%rms_err%u )
         CALL Stats_Calculate( n, 0, oi%geoamv(n)%v(k)%qc, & 
                               re%geoamv(n)%v(k), nv, & 
                               geoamv%minimum%v, geoamv%maximum%v, &
                               geoamv%average%v, geoamv%rms_err%v )

      END DO
       endif    ! end if( oi%geoamv(n)%loc%proc_domain )

      END DO

   END IF

! Do inter-processor communication to gather statistics.
   call proc_sum_int( nu )
   call proc_sum_int( nv )
   num_geoamv_tot = nu + nv
   
   call proc_stats_combine(geoamv%average%u, geoamv%rms_err%u, &
                           geoamv%minimum%u%value, geoamv%maximum%u%value, &
                           geoamv%minimum%u%n, geoamv%maximum%u%n, &
                           geoamv%minimum%u%l, geoamv%maximum%u%l )
   call proc_stats_combine(geoamv%average%v, geoamv%rms_err%v, &
                           geoamv%minimum%v%value, geoamv%maximum%v%value, &
                           geoamv%minimum%v%n, geoamv%maximum%v%n, &
                           geoamv%minimum%v%l, geoamv%maximum%v%l )
   
   
   IF ( wrf_dm_on_monitor() ) THEN
   
    IF ( nu /= 0 .or. nv /= 0 ) THEN
   
     write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Geoamv'
   
     CALL DA_Print_Stats_Geoamv( stats_unit, nu, nv, geoamv )
   
    END IF
   
   END IF

END SUBROUTINE DA_AO_Stats_GeoAMV  
