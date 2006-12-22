subroutine da_print_stats_radar( stats_unit, nrv, nrf, Radar )

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nrv, nrf
   type (stats_Radar_type), intent(in)    :: Radar
   
   write(unit=stats_unit, fmt='(a/)') &
      '   var             rv (m/s)        n    k         rf (dBZ)        n    k  '

   write(unit=stats_unit, fmt='(a,i16,i31)') &
      '  Number: ', nrv, nrf

   if (nrv < 1) nrv = 1
   if (nrf < 1) nrf = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, f17.4,i9,i5))')  &
      ' Minimum(n,k): ', Radar%minimum%rv, Radar%minimum%rf, &
      ' Maximum(n,k): ', Radar%maximum%rv, Radar%maximum%rf
   write(unit=stats_unit, fmt='((a,2(f12.4,19x)))')              &
      ' Average     : ', Radar%average%rv/real(nrv), Radar%average%rf/real(nrf), &
      '    RMSE     : ', sqrt(Radar%rms_err%rv/real(nrv)), &
      sqrt(Radar%rms_err%rf/real(nrf))

end subroutine da_print_stats_radar


