SUBROUTINE DA_Print_Stats_Radar( stats_unit, nrv, nrf, Radar )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nrv, nrf
   TYPE (stats_Radar_type), INTENT(IN)    :: Radar
   
   write(unit=stats_unit, fmt='(a/)') &
        '   var             rv (m/s)        n    k         rf (dBZ)        n    k  '

   write(unit=stats_unit, fmt='(a,i16,i31)') &
        '  Number: ', nrv, nrf

   if(nrv < 1) nrv = 1
   if(nrf < 1) nrf = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, f17.4,i9,i5))')  &
        ' Minimum(n,k): ', Radar%minimum%rv, Radar%minimum%rf, &
        ' Maximum(n,k): ', Radar%maximum%rv, Radar%maximum%rf
   write(unit=stats_unit, fmt='((a,2(f12.4,19x)))')              &
        ' Average     : ', Radar%average%rv/real(nrv), Radar%average%rf/real(nrf), &
        '    RMSE     : ', sqrt(Radar%rms_err%rv/real(nrv)), sqrt(Radar%rms_err%rf/real(nrf))

END SUBROUTINE DA_Print_Stats_Radar

