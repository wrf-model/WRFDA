SUBROUTINE DA_Print_Stats_Pilot( stats_unit, nu, nv, Pilot )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nu, nv
   TYPE (stats_pilot_type), INTENT(IN):: Pilot
   
   write(unit=stats_unit, fmt='(a/)') &
        '   var             u (m/s)     n    k    v (m/s)     n    k  '

   write(unit=stats_unit, fmt='(a,i16,i22)') &
        '  Number: ', nu, nv

   if(nu < 1) nu = 1
   if(nv < 1) nv = 1
   
   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', Pilot%minimum%u, Pilot%minimum%v, &
        ' Maximum(n,k): ', Pilot%maximum%u, Pilot%maximum%v
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))')  &
        ' Average     : ', Pilot%average%u/real(nu), &
                      Pilot%average%v/real(nv),      &
        '    RMSE     : ', sqrt(Pilot%rms_err%u/real(nu)), &
                      sqrt(Pilot%rms_err%v/real(nv))

END SUBROUTINE DA_Print_Stats_Pilot

