SUBROUTINE DA_Print_Stats_Satem( stats_unit, nthickness, Satem )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nthickness
   TYPE (stats_satem_type), INTENT(IN)  :: Satem
   
   write(unit=stats_unit, fmt='(a/)') &
        '   var           thickness(m)  n    k'  

   write(unit=stats_unit, fmt='(a,i16)') &
        '  Number: ', nthickness

   if(nthickness < 1) nthickness = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
        ' Minimum(n,k): ', Satem%minimum%thickness,    &
        ' Maximum(n,k): ', Satem%maximum%thickness
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
        ' Average     : ', Satem%average%thickness/real(nthickness),    &
        '    RMSE     : ', sqrt(Satem%rms_err%thickness/real(nthickness))

END SUBROUTINE DA_Print_Stats_Satem

