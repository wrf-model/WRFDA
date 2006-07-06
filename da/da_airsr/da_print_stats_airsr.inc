subroutine da_print_stats_airsr( stats_unit, nt, nq, airsr )
!-------------------------------------------------------------------------
!  History:
!
!     Creation:        03/07/2006                Syed RH Rizvi        
!
!     Purpose:
!              Prints out Table for AIRS diagnostics
!-------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,           INTENT(IN)    :: stats_unit
   INTEGER,           INTENT(INOUT) :: nt, nq
   TYPE (stats_airsr_type), INTENT(IN):: airsr
!-------------------------------------------------------------------------
   
  write(unit=stats_unit, fmt='(5a/)') &
    '   var             ', &
    't (K)       n    k    ', 'q (kg/kg)   n    k'

   write(unit=stats_unit, fmt='(a,i16,i22)') &
        '  Number: ', nt, nq

   if(nt < 1) nt = 1
   if(nq < 1) nq = 1
   
   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', airsr%minimum%t, airsr%minimum%q, &
        ' Maximum(n,k): ', airsr%maximum%t, airsr%maximum%q
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))')  &
        ' Average     : ', airsr%average%t/real(nt), &
                      airsr%average%q/real(nq),      &
        '    RMSE     : ', sqrt(airsr%rms_err%t/real(nt)), &
                      sqrt(airsr%rms_err%q/real(nq))

end subroutine da_print_stats_airsr
