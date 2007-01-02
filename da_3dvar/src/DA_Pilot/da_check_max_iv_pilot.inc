SUBROUTINE da_check_max_iv_pilot(it, pilot, itu,ituf,itvv,itvvf)
!-------------------------------------------------------------------------
!
!  History:
!
!     Additions:        02/26/2003                    S. R. H. Rizvi
!
!            outer iteration and extra counters are added in the  argument
!            list for generating detailed information about rejected obs in
!            various outer iterations
!
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,           INTENT (IN)         :: it      ! Outer iteration
   TYPE (pilot_type), INTENT(INOUT)       :: pilot
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf
   INTEGER                                :: i, k
   LOGICAL                                :: failed

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   IF ( check_max_iv_print )THEN
      WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'
      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for Pilot_U   = ',max_error_uv, &
     'Error max test ratio for Pilot_V   = ',max_error_uv
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   DO k = 1, pilot % info % levels

!        U-wind
         CALL da_max_error_qc(it, pilot % info, pilot % u(k), &
                 max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                               failed, check_max_iv_print)
!        V-wind
         CALL da_max_error_qc (it, pilot % info, pilot % v(k),&
                 max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                                failed, check_max_iv_print)
   END DO

END SUBROUTINE da_check_max_iv_pilot
