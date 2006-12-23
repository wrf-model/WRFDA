subroutine da_check_max_iv_ssmi_rv(it, ssmi_retrieval, &
                     itpw, itpwf, ispeed, ispeedf)

   implicit none

   integer,    intent (in)                :: it ! Outer loop 
   integer,    intent (inout)             :: itpw, itpwf, ispeed, ispeedf
   type (ssmi_retrieval_type), intent(inout)       :: ssmi_retrieval

   integer                                :: iunit ! Output unit.
   logical                                :: failed, print

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit
   print = check_max_iv_print

   if (print) then
      write (iunit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMI_rv_TPW   = ',max_error_pw, &
         'Error max test ratio for SSMI_rv_SPEED = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   ! TPW

   call da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % tpw, &
                            max_error_pw, itpw, itpwf, iunit, 'TPW  ', &
                            failed, print)

   ! SPEED

   call da_max_error_qc(it, ssmi_retrieval % info, ssmi_retrieval % speed, &
                            max_error_uv, ispeed, ispeedf, iunit, 'SPEED', &
                            failed, print)

end subroutine da_check_max_iv_ssmi_rv


