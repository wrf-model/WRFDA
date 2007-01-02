SUBROUTINE da_check_max_iv_airep(it, airep , &
                  itu,ituf,itvv,itvvf,itt,ittf )
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

   TYPE (airep_type), INTENT(INOUT)       :: airep
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf,itt,ittf
   INTEGER                                :: i, k
   LOGICAL                                :: failed

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!


   if ( check_max_iv_print ) THEN
      WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for Airep_U   = ',max_error_uv, &
     'Error max test ratio for Airep_V   = ',max_error_uv, &
     'Error max test ratio for Airep_T   = ',max_error_t
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, airep % info % levels

      if ( airep % zk(k) > 0.0) then

!        U-wind

         CALL da_max_error_qc(it, airep % info, airep % u(k), &
                 max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                               failed, check_max_iv_print)
!        V-wind

         CALL da_max_error_qc (it, airep % info, airep % v(k),&
                 max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                                failed, check_max_iv_print)                           

!        Temperature

         CALL da_max_error_qc (it, airep % info, airep % t(k),&
                 max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                                failed, check_max_iv_print)

      endif

   enddo

END SUBROUTINE da_check_max_iv_airep
