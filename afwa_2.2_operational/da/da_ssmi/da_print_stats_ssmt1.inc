subroutine da_print_stats_ssmt1(stats_unit, nt, minimum, maximum, &
                                 average, rms_err)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: stats_unit
   integer,           intent(inout) :: nt
   type (maxmin_type), intent(in)   :: minimum
   type (maxmin_type), intent(in)   :: maximum
   real, intent(in)                 :: average
   real, intent(in)                 :: rms_err
   
   write(unit=stats_unit, fmt='(a/)') '   var              T(K)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nt

   if (nt < 1) nt = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
        ' Minimum(n,k): ', minimum,    &
        ' Maximum(n,k): ', maximum
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
        ' Average     : ', average/real(nt),    &
        '    RMSE     : ', sqrt(rms_err/real(nt))

end subroutine da_print_stats_ssmt1


