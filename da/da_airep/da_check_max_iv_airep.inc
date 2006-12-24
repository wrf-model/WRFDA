subroutine da_check_max_iv_airep(it, airep , &
                  itu,ituf,itvv,itvvf,itt,ittf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent (in)         :: it      ! Outer iteration

   type (airep_type), intent(inout)       :: airep
   integer, intent(inout) :: itu,ituf,itvv,itvvf,itt,ittf
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
         'Error max test ratio for Airep_U   = ',max_error_uv, &
         'Error max test ratio for Airep_V   = ',max_error_uv, &
         'Error max test ratio for Airep_T   = ',max_error_t
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, airep % info % levels
      if (airep % zk(k) > 0.0) then
         ! U-wind
         call da_max_error_qc(it, airep % info, airep % u(k), &
                    max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', &
                                  failed, check_max_iv_print)
         ! V-wind
         call da_max_error_qc (it, airep % info, airep % v(k),&
                    max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', &
                                   failed, check_max_iv_print)                           

         ! Temperature
         call da_max_error_qc (it, airep % info, airep % t(k),&
                    max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                                   failed, check_max_iv_print)
      end if
   end do

end subroutine da_check_max_iv_airep


