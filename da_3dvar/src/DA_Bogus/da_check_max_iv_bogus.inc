SUBROUTINE da_check_max_iv_bogus(it,bogus,itu,ituf,itvv,itvvf,  &
                                 itt,ittf,itqv,itqvf,itslp,itslpf)

   IMPLICIT NONE

   INTEGER,           INTENT (IN)         :: it      ! Outer iteration
   TYPE (bogus_type), INTENT(INOUT)       :: bogus
   INTEGER,           INTENT(INOUT)       :: itu,ituf,itvv,itvvf,itslp,itslpf,&
                                             itt,ittf,itqv,itqvf
   INTEGER                                :: i, k
   LOGICAL                                :: failed

   INTEGER                                :: iunit ! Output unit.
   LOGICAL                                :: print ! Output if .TRUE

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
     'Error max test ratio for Bogus_U   = ',max_error_buv, &
     'Error max test ratio for Bogus_V   = ',max_error_buv, &
     'Error max test ratio for Bogus_T   = ',max_error_bt , &
     'Error max test ratio for Bogus_QV  = ',max_error_bq , &
     'Error max test ratio for Bogus_SLP = ',max_error_slp  
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

   do k = 1, bogus % info % levels

!     U-wind
      CALL da_max_error_qc(it, bogus % info, bogus % u(k), &
                            max_error_buv, itu, ituf, iunit, 'U    ', &
                            failed, print)
!     V-wind
      CALL da_max_error_qc(it, bogus % info, bogus % v(k), &
                            max_error_buv, itvv, itvvf, iunit, 'V    ', &
                            failed, print)                           
!     Temperature
      CALL da_max_error_qc(it, bogus % info, bogus % t(k), &
                            max_error_bt, itt, ittf, iunit, 'TEMP ', &
                            failed, print)                           
!     Mixing ratio, same ratio as relative humidity
      CALL da_max_error_qc(it, bogus % info, bogus % q(k), &
                            max_error_bq, itqv, itqvf, iunit, 'QV   ', &
                            failed, print)                           

   END DO

!       See Level Pressure

   CALL da_max_error_qc( it, bogus % info, bogus %slp,  &
                         max_error_slp, itslp, itslpf,iunit,  &
                         'SLP  ', failed, print)

END SUBROUTINE da_check_max_iv_bogus
