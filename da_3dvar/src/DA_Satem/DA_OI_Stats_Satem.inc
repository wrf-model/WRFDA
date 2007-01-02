SUBROUTINE DA_OI_Stats_Satem ( stats_unit, oi )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI

   TYPE (stats_satem_type)          :: Satem
   INTEGER                          :: nthickness
   INTEGER                          :: n, k

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   nthickness = 0
   
   Satem%maximum%thickness = maxmin_type( missing_r, 0, 0)
   Satem%minimum%thickness = maxmin_type(-missing_r, 0, 0)
   Satem%average = residual_satem1_type(0.0)
   Satem%rms_err = Satem%average

   IF ( oi%num_Satem > 0 ) THEN

      DO n=1, oi%num_Satem

       if( oi%satem(n)%loc%proc_domain ) then

        do k=1, oi%satem(n)%info%levels

         CALL Stats_Calculate( n, k, oi%Satem(n)%thickness(k)%qc, &
                          oi%Satem(n)%thickness(k)%inv, nthickness, &
                          Satem%minimum%thickness, Satem%maximum%thickness, &
                          Satem%average%thickness, Satem%rms_err%thickness)
        end do
       endif    ! end if( oi%satem(n)%loc%proc_domain )
      END DO

   END IF

! Do inter-processor communication to gather statistics.
    call proc_sum_int( nthickness )
   
    call proc_stats_combine(Satem%average%thickness, Satem%rms_err%thickness, &
             Satem%minimum%thickness%value, Satem%maximum%thickness%value, &
             Satem%minimum%thickness%n, Satem%maximum%thickness%n, &
             Satem%minimum%thickness%l, Satem%maximum%thickness%l )
   
    IF ( wrf_dm_on_monitor() ) THEN
   
     IF (nthickness /= 0 ) THEN
   
       write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for Satem'
   
       CALL DA_Print_Stats_Satem( stats_unit, nthickness, Satem )
   
     END IF
   
   END IF

END SUBROUTINE DA_OI_Stats_Satem

