SUBROUTINE da_check_max_iv_gpspw(it, gpspw, ittpw,ittpwf)
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
   TYPE (gpspw_type), INTENT(INOUT)       :: gpspw
   INTEGER, INTENT(INOUT) :: ittpw,ittpwf
   LOGICAL                                :: failed 
!
   IF ( check_max_iv_print )THEN
      WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'
      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for gps_tpw = ',max_error_pw
   endif

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

!  TPW:
   CALL da_max_error_qc(it, gpspw % info, gpspw % tpw, max_error_pw, ittpw, &
       ittpwf, check_max_iv_unit, 'TPW  ', failed, check_max_iv_print)

END SUBROUTINE da_check_max_iv_gpspw

