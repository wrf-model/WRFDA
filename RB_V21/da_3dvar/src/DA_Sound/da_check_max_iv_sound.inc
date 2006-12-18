SUBROUTINE da_check_max_iv_sound(it, sound,  &
                itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf )
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
   TYPE (sound_type), INTENT(INOUT)       :: sound
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf

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
     'Error max test ratio for Sound_U   = ',max_error_uv, &
     'Error max test ratio for Sound_V   = ',max_error_uv, &
     'Error max test ratio for Sound_T   = ',max_error_t,  &
     'Error max test ratio for Sound_QV  = ',max_error_q
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   DO k = 1, sound % info % levels

!     U-wind

      if(sound % u(k) % qc >= obs_qc_pointer ) &
         CALL da_max_error_qc(it, sound % info, sound % u(k), &
                 max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                               failed, check_max_iv_print)
!     V-wind

      if(sound % v(k) % qc >= obs_qc_pointer ) &
         CALL da_max_error_qc (it, sound % info, sound % v(k),&
                 max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                                failed, check_max_iv_print)                           

!     Temperature

      if(sound % t(k) % qc >= obs_qc_pointer ) &
         CALL da_max_error_qc (it, sound % info, sound % t(k),&
                 max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                                failed, check_max_iv_print)

!     Mixing ratio, same ratio as relative humidity

      if(sound % q(k) % qc >= obs_qc_pointer ) &
         CALL da_max_error_qc (it, sound % info, sound % q(k), &
                 max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                                failed, check_max_iv_print)
   END DO

END SUBROUTINE da_check_max_iv_sound
