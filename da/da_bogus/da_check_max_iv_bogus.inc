subroutine da_check_max_iv_bogus(it,bogus,itu,ituf,itvv,itvvf,  &
                                  itt,ittf,itqv,itqvf,itslp,itslpf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent (in)         :: it      ! Outer iteration
   type (bogus_type), intent(inout)       :: bogus
   integer,           intent(inout)       :: itu,ituf,itvv,itvvf,itslp,itslpf,&
                                             itt,ittf,itqv,itqvf
   integer                                :: k
   logical                                :: failed

   integer                                :: iunit ! Output unit.
   logical                                :: print ! Output if .true

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit
   print = check_max_iv_print

   if (print) then
      write (iunit,'(A)')  &
        '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
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
      call da_max_error_qc(it, bogus % info, bogus % u(k), &
                             max_error_buv, itu, ituf, iunit, 'U    ', &
                             failed, print)
      ! V-wind
      call da_max_error_qc(it, bogus % info, bogus % v(k), &
                             max_error_buv, itvv, itvvf, iunit, 'V    ', &
                             failed, print)                           
      ! Temperature
      call da_max_error_qc(it, bogus % info, bogus % t(k), &
                             max_error_bt, itt, ittf, iunit, 'TEMP ', &
                             failed, print)                           
      ! Mixing ratio, same ratio as relative humidity
      call da_max_error_qc(it, bogus % info, bogus % q(k), &
                             max_error_bq, itqv, itqvf, iunit, 'QV   ', &
                             failed, print)                           

   end do

   ! Sea Level Pressure

   call da_max_error_qc(it, bogus % info, bogus %slp,  &
                         max_error_slp, itslp, itslpf,iunit,  &
                         'SLP  ', failed, print)

end subroutine da_check_max_iv_bogus


