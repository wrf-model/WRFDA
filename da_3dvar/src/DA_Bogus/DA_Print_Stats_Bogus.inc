SUBROUTINE DA_Print_Stats_Bogus( stats_unit, nu, nv, nslp, Bogus )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)       :: stats_unit
   INTEGER,           INTENT(INOUT)    :: nu, nv, nslp
   TYPE (stats_bogus_type), INTENT(IN) :: Bogus
   
   write(unit=stats_unit, fmt='(a/)') &
        '   var             u (m/s)     n    k    v (m/s)     n    k  &
                         &  slp (Pa)      n    k ' 

   write(unit=stats_unit, fmt='(a,i16,2i22)') &
        '  Number: ', nu, nv, nslp

   if(nu < 1) nu = 1
   if(nv < 1) nv = 1
   if(nslp < 1) nslp = 1
   
   write(unit=stats_unit, fmt='((a,3(f12.4,2i5)))') &
        ' Minimum(n,k): ', Bogus%minimum%u, Bogus%minimum%v, Bogus%minimum%slp, &
        ' Maximum(n,k): ', Bogus%maximum%u, Bogus%maximum%v, Bogus%maximum%slp
   write(unit=stats_unit, fmt='((a,3(f12.4,10x)))') &
        ' Average     : ', Bogus%average%u/real(nu), Bogus%average%v/real(nv), &
                       Bogus%average%slp/real(nslp),  &
        '    RMSE     : ', sqrt(Bogus%rms_err%u/real(nu)), &
                      sqrt(Bogus%rms_err%v/real(nv)), &
                      sqrt(Bogus%rms_err%slp/real(nslp))

END SUBROUTINE DA_Print_Stats_Bogus

