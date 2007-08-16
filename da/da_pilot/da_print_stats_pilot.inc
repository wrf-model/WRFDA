subroutine da_print_stats_pilot(stats_unit, nu, nv, Pilot)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv
   type (stats_pilot_type), intent(in)    :: pilot

   if (trace_use_dull) call da_trace_entry("da_print_stats_pilot")

   write(unit=stats_unit, fmt='(a/)') &
      '   var             u (m/s)     n    k    v (m/s)     n    k  '

   write(unit=stats_unit, fmt='(a,i16,i22)') &
      '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
       ' Minimum(n,k): ', Pilot%minimum%u, Pilot%minimum%v, &
       ' Maximum(n,k): ', Pilot%maximum%u, Pilot%maximum%v
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))')  &
      ' Average     : ', Pilot%average%u/real(nu), &
                         Pilot%average%v/real(nv),      &
      '    RMSE     : ', sqrt(Pilot%rms_err%u/real(nu)), &
                         sqrt(Pilot%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_pilot")

end subroutine da_print_stats_pilot


