SUBROUTINE DA_Print_Stats_PolarAMV( stats_unit, nu, nv, polaramv )

   implicit none

   INTEGER,           intent(in)    :: stats_unit
   INTEGER,           intent(inout) :: nu, nv
   TYPE (stats_polaramv_type), intent(in)    :: polaramv

   write(unit=stats_unit, fmt='(a/)') &
        '   var             u (m/s)     n    k    v (m/s)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv

   if(nu < 1) nu = 1
   if(nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', polaramv%minimum%u, polaramv%minimum%v, &
        ' Maximum(n,k): ', polaramv%maximum%u, polaramv%maximum%v

   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))') &
        ' Average     : ', polaramv%average%u/real(nu), polaramv%average%v/real(nv), &
        '    RMSE     : ', sqrt(polaramv%rms_err%u/real(nu)), &
                      sqrt(polaramv%rms_err%v/real(nv))

END SUBROUTINE DA_Print_Stats_PolarAMV

