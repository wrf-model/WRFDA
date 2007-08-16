subroutine da_check_max_iv_bogus(it,bogus,itu,ituf,itvv,itvvf,  &
                                  itt,ittf,itqv,itqvf,itslp,itslpf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: it      ! Outer iteration
   type (bogus_type), intent(inout) :: bogus
   integer,           intent(inout) :: itu,ituf,itvv,itvvf,itslp,itslpf,&
                                       itt,ittf,itqv,itqvf

   integer :: k
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_bogus")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
        '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
        'Error max test ratio for Bogus_U   = ',max_error_buv, &
        'Error max test ratio for Bogus_V   = ',max_error_buv, &
        'Error max test ratio for Bogus_T   = ',max_error_bt , &
        'Error max test ratio for Bogus_QV  = ',max_error_bq , &
        'Error max test ratio for Bogus_SLP = ',max_error_slp  
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, bogus % info % levels
      ! U-wind
      call da_max_error_qc(it, bogus % info, bogus % u(k), max_error_buv, itu, ituf, &
         check_max_iv_unit, 'U    ', failed, check_max_iv_print)
      ! V-wind
      call da_max_error_qc(it, bogus % info, bogus % v(k), max_error_buv, itvv, itvvf, &
         check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
      ! Temperature
      call da_max_error_qc(it, bogus % info, bogus % t(k), max_error_bt, itt, ittf, &
         check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)                           
      ! Mixing ratio, same ratio as relative humidity
      call da_max_error_qc(it, bogus % info, bogus % q(k), max_error_bq, itqv, itqvf, &
         check_max_iv_unit, 'QV   ', failed, check_max_iv_print)                           
   end do

   ! Sea Level Pressure

   call da_max_error_qc(it, bogus % info, bogus %slp,  &
                         max_error_slp, itslp, itslpf,check_max_iv_unit,  &
                         'SLP  ', failed, check_max_iv_print)
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_bogus")

end subroutine da_check_max_iv_bogus


