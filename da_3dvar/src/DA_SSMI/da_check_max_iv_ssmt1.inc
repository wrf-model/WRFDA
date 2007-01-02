SUBROUTINE da_check_max_iv_ssmt1(it, ssmt1, itt,ittf )

   IMPLICIT NONE
   INTEGER,           INTENT (IN)         ::  it      ! outer iteration
   INTEGER,           INTENT (INOUT)      ::  itt, ittf                
   TYPE (ssmt1_type), INTENT(INOUT)       :: ssmt1

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
     'Error max test ratio for SSMT1_T   = ',max_error_t
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, ssmt1 % info % levels

!     Temperature
      CALL da_max_error_qc (it, ssmt1 % info, ssmt1 % t(k),&
              max_error_t, itt, ittf, iunit, 'TEMP ', &
                             failed, print)
   END DO

END SUBROUTINE da_check_max_iv_ssmt1

