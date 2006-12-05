subroutine da_check_max_iv_ssmt1(it, ssmt1, itt,ittf )

   implicit none
   integer,           intent (in)         ::  it      ! outer iteration
   integer,           intent (inout)      ::  itt, ittf                
   type (ssmt1_type), intent(inout)       :: ssmt1

   integer                                :: iunit ! Output unit.
   integer                                ::  k
   logical                                :: failed, print

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   iunit = check_max_iv_unit
   print = check_max_iv_print

   if ( print )then
      write (iunit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = iunit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = iunit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMT1_T   = ',max_error_t
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, ssmt1 % info % levels
      !  Temperature
      call da_max_error_qc (it, ssmt1 % info, ssmt1 % t(k),&
              max_error_t, itt, ittf, iunit, 'TEMP ', &
                             failed, print)
   end do

end subroutine da_check_max_iv_ssmt1

