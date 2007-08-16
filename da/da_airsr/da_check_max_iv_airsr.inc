subroutine da_check_max_iv_airsr(it, airsr,itt,ittf,itqv,itqvf)

   !-------------------------------------------------------------------------
   ! Purpose: Applies max error check on AIRS retrievals
   !-------------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: it      ! Outer iteration
   type (airsr_type), intent(inout) :: airsr
   integer,           intent(inout) :: itt,ittf,itqv,itqvf

   integer :: k
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_airsr")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for AIRS_T   = ',max_error_t,  &
         'Error max test ratio for AIRS_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, airsr % info % levels
      ! Temperature
      if (airsr % t(k) % qc >= obs_qc_pointer) then
         call da_max_error_qc (it, airsr % info, airsr % t(k),&
            max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                         failed, check_max_iv_print)

      end if
      ! Mixing ratio, same ratio as relative humidity
      if (airsr % q(k) % qc >= obs_qc_pointer) then
         call da_max_error_qc (it, airsr % info, airsr % q(k), &
            max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                         failed, check_max_iv_print)
      end if
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_airsr")

end subroutine da_check_max_iv_airsr


