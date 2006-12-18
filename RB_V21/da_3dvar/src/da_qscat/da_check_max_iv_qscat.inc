subroutine da_check_max_iv_qscat(it, qscat, itu, itvv, ituf, itvvf)

   IMPLICIT NONE
   INTEGER,           INTENT (IN)         :: it
   INTEGER,           INTENT (INOUT)      :: itu, itvv, ituf, itvvf
   TYPE (qscat_type), INTENT(INOUT)       :: qscat

   INTEGER                                :: iunit ! Output unit.
   INTEGER                                :: iu, ivv
   INTEGER                                :: iuf, ivf
   INTEGER                                :: i
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
     'Error max test ratio for Qscat_U   = ',max_error_uv, &
     'Error max test ratio for Qscat_V   = ',max_error_uv

   end if

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

!     U-wind

      CALL da_max_error_qc(it, qscat % info, qscat % u, &
              max_error_uv, itu, ituf, iunit, 'U    ', &
                            failed, print)
!     V-wind

      CALL da_max_error_qc (it, qscat % info, qscat % v,&
              max_error_uv, itvv, itvvf, iunit, 'V    ', &
                             failed, print)                           

!-----------------------------------------------------------------------------!
!  [3.0] Output test failed results:
!-----------------------------------------------------------------------------!
 
   IF ( print .and. failed ) THEN
      WRITE (UNIT = iunit, FMT = '(/,10(2(A,I6),/))') & 
      'Number of failed qscat u-wind observations:       ',ituf,  ' on ',itu,   &
      'Number of failed qscat v-wind observations:       ',itvvf,  ' on ',itvv
   ENDIF

end subroutine da_check_max_iv_qscat
