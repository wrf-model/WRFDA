SUBROUTINE DA_Print_Stats_Synop( stats_unit, nu, nv, nt, np, nq, synop )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nu, nv, nt, np, nq
   TYPE (stats_synop_type), INTENT(IN):: synop
   
   write(unit=stats_unit, fmt='(6a/)') &
        '   var             ', &
        'u (m/s)     n    k    ', &
        'v (m/s)     n    k    ', &
        't (K)       n    k    ', &
        'p (Pa)      n    k    ', &
        'q (kg/kg)   n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv, nt, np, nq

   if(nu < 1) nu = 1
   if(nv < 1) nv = 1
   if(nt < 1) nt = 1
   if(np < 1) np = 1
   if(nq < 1) nq = 1
   
   write(unit=stats_unit, fmt='((a,4(f12.4,2i5),e12.4,2i5))') &
        ' Minimum(n,k): ', synop%minimum%u, synop%minimum%v, synop%minimum%t, &
                           synop%minimum%p, synop%minimum%q, &
        ' Maximum(n,k): ', synop%maximum%u, synop%maximum%v, synop%maximum%t, &
                           synop%maximum%p, synop%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
        ' Average     : ', synop%average%u/real(nu), synop%average%v/real(nv), &
                           synop%average%t/real(nt), synop%average%p/real(np), &
                           synop%average%q/real(nq), &
        '    RMSE     : ', sqrt(synop%rms_err%u/real(nu)), &
                           sqrt(synop%rms_err%v/real(nv)), &
                           sqrt(synop%rms_err%t/real(nt)), &
                           sqrt(synop%rms_err%p/real(np)), &
                           sqrt(synop%rms_err%q/real(nq))

END SUBROUTINE DA_Print_Stats_Synop

