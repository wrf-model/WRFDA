SUBROUTINE da_check_max_iv_polaramv(it, polaramv, &
                           itu,ituf,itvv,itvvf)
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: Innovation vector check for Polar AMVs 
!               
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,           INTENT (IN)         :: it      ! Outer iteration
   TYPE (polaramv_type), INTENT(INOUT)      :: polaramv
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
     'Error max test ratio for Polar AMV_U   = ',max_error_uv, &
     'Error max test ratio for Polar AMV_V   = ',max_error_uv

   end if

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   DO k = 1, polaramv % info % levels

!     U-wind

      CALL da_max_error_qc(it, polaramv % info, polaramv % u(k), &
              max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                            failed, check_max_iv_print)
!     V-wind

      CALL da_max_error_qc (it, polaramv % info, polaramv % v(k),&
              max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                             failed, check_max_iv_print)                           
    
   END DO
END SUBROUTINE da_check_max_iv_polaramv      

