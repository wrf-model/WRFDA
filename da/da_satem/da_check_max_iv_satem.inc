subroutine da_check_max_iv_satem(it, satem,ith,ithf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: it       ! External iteration.
   type (satem_type), intent(inout) :: satem
   integer,           intent(inout) :: ith,ithf

   integer  :: k
   logical  :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_satem")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Satem_thickness   = ',max_error_thickness
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, satem % info % levels
      ! Thickness
      call da_max_error_qc(it, satem % info, satem % thickness(k), &
         max_error_thickness, ith, ithf, check_max_iv_unit, &
         'Thickness    ', failed, check_max_iv_print)
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_satem")

end subroutine da_check_max_iv_satem


