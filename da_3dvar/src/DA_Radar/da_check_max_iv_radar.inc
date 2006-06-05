SUBROUTINE da_check_max_iv_Radar(it, Radar, irv, irf, irvf, irff )

   IMPLICIT NONE

   INTEGER,           INTENT(IN)          :: it    
   INTEGER,           INTENT(INOUT)       :: irv, irf, irvf, irff    
   TYPE (Radar_type), INTENT(INOUT)       :: Radar

   INTEGER                                :: iunit ! Output unit.
   INTEGER                                :: i, k

   CHARACTER (LEN=80)                     :: filename
   INTEGER                                :: io_error
   LOGICAL                                :: print ! Output if .TRUE.
   LOGICAL                                :: connected
   LOGICAL                                :: failed 

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   iunit = check_max_iv_unit
   print = check_max_iv_print

   IF ( print )THEN
      filename = 'obs_max_error_check'
      INQUIRE (UNIT = iunit, OPENED = connected )

      IF ( .NOT. connected) THEN
         OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
               ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )
      ENDIF
   ENDIF

   if (.NOT.connected) THEN
      WRITE (iunit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = iunit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      IF (print) THEN
         WRITE (UNIT = iunit, FMT = '(/,9(A,F3.0,/))')  &
        'Error max test ratio for Radar_rv   = ',max_error_rv, &
        'Error max test ratio for Radar_rf   = ',max_error_rf
      ENDIF

   endif

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, Radar % info % levels

      IF ( Radar % height_qc(k) /= far_below_model_surface .AND. &
           Radar % height_qc(k) /= above_model_lid ) THEN

!        rv
       IF ( USE_Radar_rv ) THEN
         CALL da_max_error_qc_Radar(it, Radar % info, Radar % rv(k), &
                                    max_error_rv, irv, irvf, iunit, 'rv   ', &
                                    failed, print)
       ENDIF

!        rf
       IF ( USE_Radar_rf ) THEN
         CALL da_max_error_qc_Radar(it, Radar % info, Radar % rf(k),&
                                    max_error_rf, irf, irff, iunit, 'rf   ', &
                                    failed, print)
       ENDIF

      END IF

   END DO

END SUBROUTINE da_check_max_iv_Radar
