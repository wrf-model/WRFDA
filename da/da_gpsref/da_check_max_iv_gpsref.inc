subroutine da_check_max_iv_gpsref(it, gpsref, pref, fref )

   IMPLICIT NONE

   INTEGER,            INTENT(IN)    :: it       ! External iteration.
   TYPE (gpsref_type), INTENT(INOUT) :: gpsref
   INTEGER,            INTENT(INOUT) :: pref, fref
   INTEGER                           :: iunit ! Output unit.
   INTEGER                           :: k
   LOGICAL                           :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit

   IF ( check_max_iv_print ) THEN
      WRITE (iunit,'(A)')  &
         '----------------------------------------------------------------'
      WRITE (UNIT = iunit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = iunit, FMT = '(/,9(A,F3.0,/))')  &
            'Error max test ratio for GPSRef = ', max_error_ref
   ENDIF

   !----------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   failed = .FALSE.

   do k = 1, gpsref % info % levels
      call da_max_error_qc(it, gpsref % info, gpsref % ref(k), &
           max_error_ref, pref, fref, iunit, &
           'GPSRef    ', failed, check_max_iv_print)
   END DO

end subroutine da_check_max_iv_gpsref

