SUBROUTINE da_check_max_iv_sonde_sfc(it, sonde_sfc, &
             itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf )
!-------------------------------------------------------------------------
!
!  History:
!
!     Additions:        07/08/2003                    S. R. H. Rizvi
!
!            outer iteration and extra counters are added in the  argument
!            list for generating detailed information about rejected obs in
!            various outer iterations
!
!-------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER,           INTENT (IN)         :: it      ! Outer iteration
   TYPE (synop_type), INTENT(INOUT)       :: sonde_sfc
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf
!
   INTEGER                                :: i
   LOGICAL                                :: failed

!-----------------------------------------------------------------------------!
!  [1.0] Open diagnostic file:
!-----------------------------------------------------------------------------!

   IF ( check_max_iv_print )THEN
      WRITE (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for Sonde_sfc_U   = ',max_error_uv, &
     'Error max test ratio for Sonde_sfc_V   = ',max_error_uv, &
     'Error max test ratio for Sonde_sfc_P   = ',max_error_p,  &
     'Error max test ratio for Sonde_sfc_T   = ',max_error_t,  &
     'Error max test ratio for Sonde_sfc_QV  = ',max_error_q
   ENDIF

!-----------------------------------------------------------------------------!
!  [2.0] Perform maximum innovation vector check:
!-----------------------------------------------------------------------------!

   failed = .FALSE.

!  U-wind

   CALL da_max_error_qc(it, sonde_sfc % info, sonde_sfc % u, &
           max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                         failed, check_max_iv_print)
!  V-wind

   CALL da_max_error_qc (it, sonde_sfc % info, sonde_sfc % v,&
           max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                          failed, check_max_iv_print)                           
!  Pressure

   CALL da_max_error_qc (it, sonde_sfc % info, sonde_sfc % p,&
           max_error_p, itp, itpf, check_max_iv_unit, 'Press', &
                          failed, check_max_iv_print)         

!  Temperature

   CALL da_max_error_qc (it, sonde_sfc % info, sonde_sfc % t,&
           max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                          failed, check_max_iv_print)

!  Mixing ratio, same ratio as relative humidity

   CALL da_max_error_qc (it, sonde_sfc % info, sonde_sfc % q, &
           max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                          failed, check_max_iv_print)

END SUBROUTINE da_check_max_iv_sonde_sfc

