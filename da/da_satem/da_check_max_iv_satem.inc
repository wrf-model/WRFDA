subroutine da_check_max_iv_satem(it, satem,ith,ithf )

   implicit none

   integer, intent(in)            :: it       ! External iteration.
   type (satem_type), intent(inout)       :: satem
   integer, intent(inout)                 :: ith,ithf
   integer                                :: iunit ! Output unit.
   integer                                :: k
   logical                                :: failed, print

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit
   print = check_max_iv_print

   if ( print )then
      write (iunit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Satem_thickness   = ',max_error_thickness
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, satem % info % levels
      ! Thickness
      call da_max_error_qc(it, satem % info, satem % thickness(k), &
         max_error_thickness, ith, ithf, iunit, &
         'Thickness    ', failed, print)
   end do

end subroutine da_check_max_iv_satem

