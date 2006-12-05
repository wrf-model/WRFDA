subroutine da_check_max_iv_sound(it, sound,  &
                itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf )

   implicit none

   integer,           intent (in)         :: it      ! Outer iteration
   type (sound_type), intent(inout)       :: sound
   integer, intent(inout) :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf

   integer                                :: k
   logical                                :: failed

   !--------------------------------------------------------------------------- 
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if ( check_max_iv_print )then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Sound_U   = ',max_error_uv, &
         'Error max test ratio for Sound_V   = ',max_error_uv, &
         'Error max test ratio for Sound_T   = ',max_error_t,  &
         'Error max test ratio for Sound_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, sound % info % levels
      ! U-wind

      if (sound % u(k) % qc >= obs_qc_pointer ) then
         call da_max_error_qc(it, sound % info, sound % u(k), &
                  max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                                failed, check_max_iv_print)
      end if

      ! V-wind

      if (sound % v(k) % qc >= obs_qc_pointer ) then
         call da_max_error_qc (it, sound % info, sound % v(k),&
                  max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                                 failed, check_max_iv_print)                           
      end if

      ! Temperature

      if (sound % t(k) % qc >= obs_qc_pointer ) then
         call da_max_error_qc (it, sound % info, sound % t(k),&
                  max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                                 failed, check_max_iv_print)
      end if

      ! Mixing ratio, same ratio as relative humidity

      if (sound % q(k) % qc >= obs_qc_pointer ) then
         call da_max_error_qc (it, sound % info, sound % q(k), &
                  max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                                 failed, check_max_iv_print)
      end if
   end do

end subroutine da_check_max_iv_sound
