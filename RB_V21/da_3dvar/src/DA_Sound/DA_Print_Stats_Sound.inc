SUBROUTINE DA_Print_Stats_Sound( stats_unit, nu, nv, nt, nq, Sound )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nu, nv, nt, nq
   TYPE (stats_sound_type), INTENT(IN)    :: Sound
   
      write(unit=stats_unit, fmt='(5a/)') &
        '   var             ', &
        'u (m/s)     n    k    ', &
        'v (m/s)     n    k    ', &
        't (K)       n    k    ', &
        'q (kg/kg)   n    k'

      write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv, nt, nq

      if(nu < 1) nu = 1
      if(nv < 1) nv = 1
      if(nt < 1) nt = 1
      if(nq < 1) nq = 1

      write(unit=stats_unit, fmt='((a,3(f12.4,2i5),e12.4,2i5))') &
        ' Minimum(n,k): ', Sound%minimum%u, Sound%minimum%v, &
                           Sound%minimum%t, Sound%minimum%q, &
        ' Maximum(n,k): ', Sound%maximum%u, Sound%maximum%v, &
                           Sound%maximum%t, Sound%maximum%q
      write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x))') &
        ' Average     : ', Sound%average%u/real(nu), &
                           Sound%average%v/real(nv), &
                           Sound%average%t/real(nt), &
                           Sound%average%q/real(nq), &
        '    RMSE     : ', sqrt(Sound%rms_err%u/real(nu)), &
                           sqrt(Sound%rms_err%v/real(nv)), &
                           sqrt(Sound%rms_err%t/real(nt)), &
                           sqrt(Sound%rms_err%q/real(nq))

END SUBROUTINE DA_Print_Stats_Sound

