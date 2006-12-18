SUBROUTINE DA_Print_Stats_GPSRef( stats_unit, ngpsref, GPSRef )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: ngpsref
   TYPE (stats_gpsref_type), INTENT(IN)  :: GPSRef
   
   write(unit=stats_unit, fmt='(a/)') &
        '   var           ref(m)  n    k'  

   write(unit=stats_unit, fmt='(a,i16)') &
        '  Number: ', ngpsref

   if(ngpsref < 1) ngpsref = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
        ' Minimum(n,k): ', GPSRef%minimum%ref,    &
        ' Maximum(n,k): ', GPSRef%maximum%ref
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
        ' Average     : ', GPSRef%average%ref/real(ngpsref),    &
        '    RMSE     : ', sqrt(GPSRef%rms_err%ref/real(ngpsref))

END SUBROUTINE DA_Print_Stats_GPSRef

