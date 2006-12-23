subroutine da_check_max_iv_gpsref(it, gpsref, pref, fref)

   implicit none

   integer,            intent(in)    :: it       ! External iteration.
   type (gpsref_type), intent(inout) :: gpsref
   integer,            intent(inout) :: pref, fref
   integer                           :: iunit ! Output unit.
   integer                           :: k
   logical                           :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit

   if (check_max_iv_print) then
      write (iunit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
            'Error max test ratio for GPSRef = ', max_error_ref
   end if

   !----------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   failed = .false.

   do k = 1, gpsref % info % levels
      call da_max_error_qc(it, gpsref % info, gpsref % ref(k), &
           max_error_ref, pref, fref, iunit, &
           'GPSRef    ', failed, check_max_iv_print)
   end do

end subroutine da_check_max_iv_gpsref


