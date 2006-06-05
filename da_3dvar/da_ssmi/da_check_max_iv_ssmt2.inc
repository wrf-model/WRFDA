SUBROUTINE da_check_max_iv_ssmt2(it, ssmt2, irh,irhf )

   IMPLICIT NONE

   INTEGER, INTENT(IN)                    :: it     ! outer iteration
   INTEGER, INTENT(INOUT)                 :: irh, irhf
   TYPE (ssmt2_type), INTENT(INOUT)       :: ssmt2

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
     'Error max test ratio for SSMT2_RH  = ',max_error_rh
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, ssmt2 % info % levels

!     Temperature
      CALL da_max_error_qc (it, ssmt2 % info, ssmt2 % rh(k),&
                             max_error_t, irh, irhf, iunit, 'RH  ', &
                             failed, print)
   END DO

END SUBROUTINE da_check_max_iv_ssmt2

