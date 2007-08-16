subroutine da_check_max_iv_polaramv(it, polaramv, &
                           itu,ituf,itvv,itvvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,              intent(in)    :: it      ! Outer iteration
   type (polaramv_type), intent(inout) :: polaramv
   integer,              intent(inout) :: itu,ituf,itvv,itvvf

   integer  :: k
   logical  :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_polaramv")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Polar AMV_U   = ',max_error_uv, &
         'Error max test ratio for Polar AMV_V   = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, polaramv % info % levels
      ! U-wind
      call da_max_error_qc(it, polaramv % info, polaramv % u(k), max_error_uv, itu, ituf, &
         check_max_iv_unit, 'U    ', failed, check_max_iv_print)

      ! V-wind
      call da_max_error_qc (it, polaramv % info, polaramv % v(k), max_error_uv, itvv, itvvf, &
         check_max_iv_unit, 'V    ', failed, check_max_iv_print)                              
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_polaramv")

end subroutine da_check_max_iv_polaramv      


