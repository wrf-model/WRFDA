subroutine da_check_max_iv_ssmt1(it, ssmt1, itt,ittf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   integer,           intent (in)         ::  it      ! outer iteration
   integer,           intent (inout)      ::  itt, ittf                
   type (ssmt1_type), intent(inout)       :: ssmt1

   integer                                ::  k
   logical                                :: failed

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMT1_T   = ',max_error_t
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do k = 1, ssmt1 % info % levels
      !  Temperature
      call da_max_error_qc (it, ssmt1 % info, ssmt1 % t(k),&
              max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                             failed, check_max_iv_print)
   end do

end subroutine da_check_max_iv_ssmt1


