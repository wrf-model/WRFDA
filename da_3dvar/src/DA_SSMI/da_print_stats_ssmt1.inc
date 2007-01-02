subroutine da_print_stats_ssmt1( stats_unit, nt, minimum, maximum, &
                                 average, rms_err )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nt
   TYPE (maxmin_type), INTENT(IN)   :: minimum
   TYPE (maxmin_type), INTENT(IN)   :: maximum
   real, INTENT(IN)                 :: average
   real, INTENT(IN)                 :: rms_err
   
   write(unit=stats_unit, fmt='(a/)') '   var              T(K)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nt

   if(nt < 1) nt = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
        ' Minimum(n,k): ', minimum,    &
        ' Maximum(n,k): ', maximum
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
        ' Average     : ', average/real(nt),    &
        '    RMSE     : ', sqrt(rms_err/real(nt))

END subroutine da_print_stats_ssmt1

