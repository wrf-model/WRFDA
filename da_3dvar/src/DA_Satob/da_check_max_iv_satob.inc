SUBROUTINE da_check_max_iv_satob(it, satob, &
                           itu,ituf,itvv,itvvf)
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
   TYPE (satob_type), INTENT(INOUT)       :: satob
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf

   LOGICAL                                :: failed
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   IF ( check_max_iv_print )THEN
      WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for Satob_U   = ',max_error_uv, &
     'Error max test ratio for Satob_V   = ',max_error_uv, &
     'Error max test ratio for Satob_P   = ',max_error_p,  &
     'Error max test ratio for Satob_T   = ',max_error_t,  &
     'Error max test ratio for Satob_QV  = ',max_error_q

   end if

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

!     U-wind

      CALL da_max_error_qc(it, satob % info, satob % u, &
              max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                            failed, check_max_iv_print)
!     V-wind

      CALL da_max_error_qc (it, satob % info, satob % v,&
              max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                             failed, check_max_iv_print)                           
END SUBROUTINE da_check_max_iv_satob

