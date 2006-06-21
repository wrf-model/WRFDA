subroutine da_check_max_iv_ships(it, ships, &
             itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf )

   IMPLICIT NONE

   INTEGER,           INTENT (IN)         :: it      ! Outer iteration
   TYPE (synop_type), INTENT(INOUT)       :: ships
   INTEGER, INTENT(INOUT) :: itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf
   LOGICAL                                :: failed

   !----------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   IF ( check_max_iv_print )THEN
      WRITE (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      WRITE (UNIT = check_max_iv_unit, FMT = '(A,/)') 'MAX ERROR TEST QC:'

      WRITE (UNIT = check_max_iv_unit, FMT = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Ships_U   = ',max_error_uv, &
         'Error max test ratio for Ships_V   = ',max_error_uv, &
         'Error max test ratio for Ships_P   = ',max_error_p,  &
         'Error max test ratio for Ships_T   = ',max_error_t,  &
         'Error max test ratio for Ships_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .FALSE.

   ! U-wind
   call da_max_error_qc(it, ships % info, ships % u, &
              max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                            failed, check_max_iv_print)
   ! V-wind
   call da_max_error_qc (it, ships % info, ships % v,&
              max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                             failed, check_max_iv_print)                           
   ! Pressure
   call da_max_error_qc (it, ships % info, ships % p,&
              max_error_p, itp, itpf, check_max_iv_unit, 'Press', &
                             failed, check_max_iv_print)         

   ! Temperature
   call da_max_error_qc (it, ships % info, ships % t,&
              max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                             failed, check_max_iv_print)

   ! Mixing ratio, same ratio as relative humidity
   call da_max_error_qc (it, ships % info, ships % q, &
              max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                             failed, check_max_iv_print)

end subroutine da_check_max_iv_ships
