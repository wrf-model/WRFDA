SUBROUTINE da_check_max_iv_gpsref(it, gpsref, pref, fref )
!
! --------------------------------------------------------------------------
!  Called by DA_Gpsref/da_get_innov_vector_Gpsref.inc
!
!                                               Y.-R. Guo  06/02/2004
! --------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,            INTENT(IN)    :: it       ! External iteration.
   TYPE (gpsref_type), INTENT(INOUT) :: gpsref
   INTEGER,            INTENT(INOUT) :: pref, fref
   INTEGER                           :: iunit ! Output unit.
   INTEGER                           :: i, k
   LOGICAL                           :: failed

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   iunit = check_max_iv_unit

   IF ( check_max_iv_print ) THEN
      WRITE (iunit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = iunit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = iunit, FMT = '(/,9(A,F3.0,/))')  &
            'Error max test ratio for GPSRef = ', max_error_ref
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, gpsref % info % levels
      CALL da_max_error_qc(it, gpsref % info, gpsref % ref(k), &
           max_error_ref, pref, fref, iunit, &
           'GPSRef    ', failed, check_max_iv_print)
   END DO

END SUBROUTINE da_check_max_iv_gpsref

