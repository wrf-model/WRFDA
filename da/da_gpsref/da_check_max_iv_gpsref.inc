subroutine da_check_max_iv_gpsref(it, gpsref, pref, fref)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,            intent(in)    :: it       ! External iteration.
   type (gpsref_type), intent(inout) :: gpsref
   integer,            intent(inout) :: pref, fref
   integer                           :: k
   logical                           :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpsref")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for GPSRef = ', max_error_ref
   end if

   !----------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   failed = .false.

   do k = 1, gpsref % info % levels
      call da_max_error_qc(it, gpsref % info, gpsref % ref(k), &
         max_error_ref, pref, fref, check_max_iv_unit, &
         'GPSRef    ', failed, check_max_iv_print)
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpsref")

end subroutine da_check_max_iv_gpsref


