subroutine da_check_max_iv_ssmt2(it, ssmt2, irh,irhf)

   implicit none

   integer, intent(in)                    :: it     ! outer iteration
   integer, intent(inout)                 :: irh, irhf
   type (ssmt2_type), intent(inout)       :: ssmt2

   integer                                :: iunit ! Output unit.
   integer                                :: k
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
         'Error max test ratio for SSMT2_RH  = ',max_error_rh
   end if

   !---------------------------------------------------------------------------
   !  [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, ssmt2 % info % levels
      ! Temperature
      call da_max_error_qc (it, ssmt2 % info, ssmt2 % rh(k),&
                             max_error_t, irh, irhf, iunit, 'RH  ', &
                             failed, print)
   end do

end subroutine da_check_max_iv_ssmt2


