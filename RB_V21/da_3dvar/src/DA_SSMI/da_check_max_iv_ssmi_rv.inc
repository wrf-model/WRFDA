SUBROUTINE da_check_max_iv_ssmi_rv(it, ssmi_retrieval, &
                     itpw, itpwf, ispeed, ispeedf)

   IMPLICIT NONE

   INTEGER,    INTENT (IN)                :: it ! Outer loop 
   INTEGER,    INTENT (INOUT)             :: itpw, itpwf, ispeed, ispeedf
   TYPE (ssmi_retrieval_type), INTENT(INOUT)       :: ssmi_retrieval

   INTEGER                                :: iunit ! Output unit.
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
     'Error max test ratio for SSMI_rv_TPW   = ',max_error_pw, &
     'Error max test ratio for SSMI_rv_SPEED = ',max_error_uv
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

!  TPW

   CALL da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % tpw, &
                            max_error_pw, itpw, itpwf, iunit, 'TPW  ', &
                            failed, print)

!  SPEED

   CALL da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % speed, &
                            max_error_uv, ispeed, ispeedf, iunit, 'SPEED', &
                            failed, print)

END SUBROUTINE da_check_max_iv_ssmi_rv
