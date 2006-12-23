subroutine da_check_max_iv_radar(it, Radar, irv, irf, irvf, irff)

   implicit none

   integer,           intent(in)          :: it    
   integer,           intent(inout)       :: irv, irf, irvf, irff    
   type (Radar_type), intent(inout)       :: Radar

   integer                                :: iunit ! Output unit.
   integer                                :: k

   character (LEN=80)                     :: filename
   integer                                :: io_error
   logical                                :: print ! Output if .true.
   logical                                :: connected
   logical                                :: failed 

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit
   print = check_max_iv_print

   if (print) then
      filename = 'obs_max_error_check'
      inQUIRE (unit = iunit, openED = connected)

      if (.NOT. connected) then
         open (unit = iunit , FILE = filename , FORM = 'FORMATTED'  , &
               ACTION = 'write' , STATUS = 'REPLACE', iostat = io_error)
      end if
   end if

   if (.NOT.connected) then
      write (iunit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      if (print) then
         write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
            'Error max test ratio for Radar_rv   = ',max_error_rv, &
           'Error max test ratio for Radar_rf   = ',max_error_rf
      end if
   end if

   !------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !------------------------------------------------------------------------

   failed = .false.

   do k = 1, Radar % info % levels
      if (Radar % height_qc(k) /= far_below_model_surface .AND. &
           Radar % height_qc(k) /= above_model_lid) then

         ! rv
         if (use_Radar_rv) then
            call da_max_error_qc_Radar(it, Radar % info, Radar % rv(k), &
                                    max_error_rv, irv, irvf, iunit, 'rv   ', &
                                    failed, print)
         end if

         ! rf
         if (use_Radar_rf) then
            call da_max_error_qc_Radar(it, Radar % info, Radar % rf(k),&
                                    max_error_rf, irf, irff, iunit, 'rf   ', &
                                    failed, print)
         end if
      end if
   end do

end subroutine da_check_max_iv_Radar


