subroutine da_check_max_iv_airsr(it, airsr,itt,ittf,itqv,itqvf)

!-------------------------------------------------------------------------
!  History:
!
!     Creation:        03/07/2006                Syed RH Rizvi        
!
!     Purpose: 
!               Applied Max error check on
!               AIRS retrievals
!-------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER,           INTENT (IN)         :: it      ! Outer iteration
  TYPE (airsr_type), INTENT(INOUT)       :: airsr
  INTEGER, INTENT(INOUT)                 :: itt,ittf,itqv,itqvf

  INTEGER                                :: i, k
  LOGICAL                                :: failed
  !-----------------------------------------------------------------------------!  
  ! [1.0] Open diagnostic file:
  !-----------------------------------------------------------------------------!

  IF ( check_max_iv_print )THEN
    WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
    WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

    WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
      'Error max test ratio for AIRS_T   = ',max_error_t,  &
      'Error max test ratio for AIRS_QV  = ',max_error_q
  ENDIF

  !-----------------------------------------------------------------------------!
  ! [2.0] Perform maximum innovation vector check:
  !-----------------------------------------------------------------------------!

  failed = .FALSE.

  DO k = 1, airsr % info % levels
    ! Temperature
    if (airsr % t(k) % qc >= obs_qc_pointer ) THEN
      CALL da_max_error_qc (it, airsr % info, airsr % t(k),&
           max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                        failed, check_max_iv_print)

    END IF
    ! Mixing ratio, same ratio as relative humidity
    if (airsr % q(k) % qc >= obs_qc_pointer ) THEN
      CALL da_max_error_qc (it, airsr % info, airsr % q(k), &
           max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                        failed, check_max_iv_print)
    END IF
  END DO

END subroutine da_check_max_iv_airsr
