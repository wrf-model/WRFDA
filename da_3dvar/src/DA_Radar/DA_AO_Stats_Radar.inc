SUBROUTINE DA_AO_Stats_Radar( stats_unit, oi, re )

   IMPLICIT NONE

   INTEGER,        INTENT (in)      :: stats_unit    ! Output unit for stats.
   TYPE (ob_type), INTENT (in)      :: oi            ! OI
   TYPE ( y_type), INTENT (in)      :: re            ! A - O

   TYPE (stats_Radar_type)          :: Radar
   INTEGER                          :: nrv, nrf
   INTEGER                          :: n, k

   IF ( oi%num_Radar > 0 ) THEN

      nrv = 0
      nrf = 0

      Radar%maximum%rv = maxmin_type(0.0, 0, 0 )
      Radar%maximum%rf = maxmin_type(0.0, 0, 0 )

      Radar%minimum = Radar%maximum
      Radar%average = residual_Radar1_type(0.0, 0.0)
      Radar%rms_err = Radar%average

      DO n=1, oi%num_Radar
      do k=1, oi%Radar(n)%info%levels

         IF ( USE_Radar_rv ) THEN
         CALL Stats_Calculate( n, k, oi%Radar(n)%rv(k)%qc, & 
                               re%Radar(n)%rv(k), nrv, & 
                               Radar%minimum%rv, Radar%maximum%rv, &
                               Radar%average%rv, Radar%rms_err%rv )
         ENDIF

         IF ( USE_Radar_rf ) THEN
         CALL Stats_Calculate( n, k, oi%Radar(n)%rf(k)%qc, & 
                               re%Radar(n)%rf(k), nrf, & 
                               Radar%minimum%rf, Radar%maximum%rf, &
                               Radar%average%rf, Radar%rms_err%rf )
         ENDIF

      end do
      END DO

      IF ( nrv /= 0 .or. nrf /= 0 ) THEN
  
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for Radar'

         call DA_Print_Stats_Radar(stats_unit, nrv, nrf, Radar)

      END IF

   END IF

END SUBROUTINE DA_AO_Stats_Radar
