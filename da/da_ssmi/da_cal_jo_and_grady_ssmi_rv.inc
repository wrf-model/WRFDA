subroutine da_cal_jo_and_grady_ssmi_rv( oi, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: oi          ! Ob Inc. structure.
   type (y_type), intent(in)    :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n

   jo % ssmir_Speed=0.0
   jo % ssmir_tpw=0.0

   do n=1, oi%num_ssmi_retrieval
      jo_grad_y%ssmi_retrieval(n)%Speed = - re%ssmi_retrieval(n)%Speed / &
                                           ( oi%ssmi_retrieval(n)%Speed%error * &
                                             oi%ssmi_retrieval(n)%Speed%error )

      jo_grad_y%ssmi_retrieval(n)%tpw = -re%ssmi_retrieval(n)%tpw / &
                                         ( oi%ssmi_retrieval(n)%tpw%error * &
                                           oi%ssmi_retrieval(n)%tpw%error )

      if (oi%ssmi_retrieval(n)%loc%proc_domain) then

         jo % ssmir_Speed = jo % ssmir_Speed - &
                        re%ssmi_retrieval(n)%Speed * &
                        jo_grad_y%ssmi_retrieval(n)%Speed

        jo % ssmir_tpw = jo % ssmir_tpw - &
                      re%ssmi_retrieval(n)%tpw * jo_grad_y%ssmi_retrieval(n)%tpw

      end if
   end do
   
   jo % ssmir_Speed = 0.5 * jo % ssmir_Speed
   jo % ssmir_tpw   = 0.5 * jo % ssmir_tpw

end subroutine da_cal_jo_and_grady_ssmi_rv

