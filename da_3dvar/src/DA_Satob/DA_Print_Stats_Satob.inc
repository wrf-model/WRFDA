SUBROUTINE DA_Print_Stats_Satob( stats_unit, nu, nv, satob )

   implicit none

   INTEGER,           intent(in)    :: stats_unit
   INTEGER,           intent(inout) :: nu, nv
   TYPE (stats_type), intent(in)    :: satob

   write(unit=stats_unit, fmt='(a/)') &
        '   var             u (m/s)     n    k    v (m/s)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv

   if(nu < 1) nu = 1
   if(nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', satob%minimum%u, satob%minimum%v, &
        ' Maximum(n,k): ', satob%maximum%u, satob%maximum%v

   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))') &
        ' Average     : ', satob%average%u/real(nu), satob%average%v/real(nv), &
        '    RMSE     : ', sqrt(satob%rms_err%u/real(nu)), &
                      sqrt(satob%rms_err%v/real(nv))

END SUBROUTINE DA_Print_Stats_Satob

