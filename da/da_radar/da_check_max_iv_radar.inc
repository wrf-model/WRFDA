subroutine da_check_max_iv_radar(it, Radar, irv, irf, irvf, irff)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)          :: it    
   integer,           intent(inout)       :: irv, irf, irvf, irff    
   type (radar_type), intent(inout)       :: Radar

   integer                                :: iunit ! Output unit.
   integer                                :: k

   logical                                :: failed 

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit

   if (check_max_iv_print) then
      write (iunit,'(a)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
           'Error max test ratio for Radar_rv   = ',max_error_rv, &
           'Error max test ratio for Radar_rf   = ',max_error_rf
   endif

   !------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !------------------------------------------------------------------------

   failed = .false.

   do k = 1, Radar % info % levels

      if (Radar % height_qc(k) /= far_below_model_surface .and. &
           Radar % height_qc(k) /= above_model_lid) then

         ! rv
         if (use_Radar_rv) then
            call da_max_error_qc_Radar(it, Radar % info, Radar % rv(k), &
                                    max_error_rv, irv, irvf, iunit, 'rv   ', &
                                    failed, check_max_iv_print)
         endif

         ! rf
         if (use_Radar_rf) then
            call da_max_error_qc_Radar(it, Radar % info, Radar % rf(k),&
                                    max_error_rf, irf, irff, iunit, 'rf   ', &
                                    failed, check_max_iv_print)
         endif
      end if
   end do

end subroutine da_check_max_iv_radar



