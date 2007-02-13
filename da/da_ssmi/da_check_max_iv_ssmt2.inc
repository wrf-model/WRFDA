subroutine da_check_max_iv_ssmt2(it, ssmt2, irh,irhf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                    :: it     ! outer iteration
   integer, intent(inout)                 :: irh, irhf
   type (ssmt2_type), intent(inout)       :: ssmt2

   integer                                :: k
   logical                                :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMT2_RH  = ',max_error_rh
   end if

   !---------------------------------------------------------------------------
   !  [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, ssmt2 % info % levels
      ! Temperature
      call da_max_error_qc (it, ssmt2 % info, ssmt2 % rh(k),&
                             max_error_t, irh, irhf, check_max_iv_unit, 'RH  ', &
                             failed, check_max_iv_print)
   end do

end subroutine da_check_max_iv_ssmt2


