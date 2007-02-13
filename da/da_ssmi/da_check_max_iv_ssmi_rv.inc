subroutine da_check_max_iv_ssmi_rv(it, ssmi_retrieval, &
                     itpw, itpwf, ispeed, ispeedf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,    intent (in)                :: it ! Outer loop 
   integer,    intent (inout)             :: itpw, itpwf, ispeed, ispeedf
   type (ssmi_retrieval_type), intent(inout)       :: ssmi_retrieval

   logical                                :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMI_rv_TPW   = ',max_error_pw, &
         'Error max test ratio for SSMI_rv_SPEED = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   ! TPW

   call da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % tpw, &
                            max_error_pw, itpw, itpwf, check_max_iv_unit, 'TPW  ', &
                            failed, check_max_iv_print)

   ! SPEED

   call da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % speed, &
                            max_error_uv, ispeed, ispeedf, check_max_iv_unit, 'SPEED', &
                            failed, check_max_iv_print)

end subroutine da_check_max_iv_ssmi_rv


