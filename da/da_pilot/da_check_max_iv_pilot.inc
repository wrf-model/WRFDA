subroutine da_check_max_iv_pilot(it, pilot, itu,ituf,itvv,itvvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent (in)         :: it      ! Outer iteration
   type (pilot_type), intent(inout)       :: pilot
   integer, intent(inout) :: itu,ituf,itvv,itvvf
   integer                                :: k
   logical                                :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Pilot_U   = ',max_error_uv, &
         'Error max test ratio for Pilot_V   = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, pilot % info % levels
      ! U-wind
      call da_max_error_qc(it, pilot % info, pilot % u(k), &
         max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
         failed, check_max_iv_print)

      ! V-wind
      call da_max_error_qc (it, pilot % info, pilot % v(k),&
         max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
         failed, check_max_iv_print)
   end do

end subroutine da_check_max_iv_pilot


