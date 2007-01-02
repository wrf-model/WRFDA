SUBROUTINE da_check_max_iv_profiler(it, profiler , &
                        itu,ituf,itvv,itvvf )
!-------------------------------------------------------------------------
!
!  History:
!
!     Created:          01/13/2003                    S. R. H. Rizvi
!
!     Additions:        03/19/2003                    S. R. H. Rizvi
!
!            Outer iteration argument is added for generating information 
!            about the rejected obs in various outer iterations 
!-------------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER,           INTENT (IN)         :: it      ! Outer iteration 
   TYPE (pilot_type), INTENT(INOUT)       :: profiler
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
     'Error max test ratio for Profiler_U   = ',max_error_uv, &
     'Error max test ratio for Profiler_V   = ',max_error_uv
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, profiler % info % levels

!        U-wind
         CALL da_max_error_qc(it, profiler % info, profiler % u(k), &
                 max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                       failed, check_max_iv_print)
!        V-wind
         CALL da_max_error_qc (it, profiler % info, profiler % v(k),&
                 max_error_uv, itvv, itvvf, check_max_iv_unit, 'V   ', &
                        failed, check_max_iv_print)
   END DO

END SUBROUTINE da_check_max_iv_profiler
