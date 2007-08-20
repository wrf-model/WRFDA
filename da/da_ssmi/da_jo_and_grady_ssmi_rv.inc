subroutine da_jo_and_grady_ssmi_rv(oi, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: oi          ! Ob Inc. structure.
   type (y_type),  intent(in)    :: re          ! Residual structure.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer  :: n

   if (trace_use) call da_trace_entry("da_jo_and_grady_ssmi_rv")

   jo % ssmir_speed = 0.0
   jo % ssmir_tpw   = 0.0

   do n=1, oi%nlocal(ssmi_rv)
      jo_grad_y%ssmi_rv(n)%speed = - re%ssmi_rv(n)%speed / &
         (oi%ssmi_rv(n)%speed%error * oi%ssmi_rv(n)%speed%error)

      jo_grad_y%ssmi_rv(n)%tpw = -re%ssmi_rv(n)%tpw / &
          (oi%ssmi_rv(n)%tpw%error * oi%ssmi_rv(n)%tpw%error)

      if (oi%ssmi_rv(n)%loc%proc_domain) then

         jo%ssmir_speed = jo%ssmir_speed - re%ssmi_rv(n)%speed * jo_grad_y%ssmi_rv(n)%speed
         jo%ssmir_tpw   = jo%ssmir_tpw   - re%ssmi_rv(n)%tpw   * jo_grad_y%ssmi_rv(n)%tpw
      end if
   end do
   
   jo % ssmir_speed = 0.5 * jo % ssmir_speed
   jo % ssmir_tpw   = 0.5 * jo % ssmir_tpw

   if (trace_use) call da_trace_exit("da_jo_and_grady_ssmi_rv")

end subroutine da_jo_and_grady_ssmi_rv


