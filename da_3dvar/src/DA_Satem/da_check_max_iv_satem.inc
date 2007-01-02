SUBROUTINE da_check_max_iv_satem(it, satem,ith,ithf )

   IMPLICIT NONE

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (satem_type), INTENT(INOUT)       :: satem
   INTEGER, INTENT(INOUT)                 :: ith,ithf
   INTEGER                                :: iunit ! Output unit.
   INTEGER                                :: i, k
   LOGICAL                                :: failed, print

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   iunit = check_max_iv_unit
   print = check_max_iv_print

   IF ( print )THEN
      WRITE (iunit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = iunit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = iunit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for Satem_thickness   = ',max_error_thickness
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, satem % info % levels

!        Thickness
         CALL da_max_error_qc(it, satem % info, satem % thickness(k), &
              max_error_thickness, ith, ithf, iunit, &
              'Thickness    ', failed, print)
   END DO

END SUBROUTINE da_check_max_iv_satem

