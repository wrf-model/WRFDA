subroutine da_cal_jo_and_grady_ssmi( oi, re, jo, jo_ssmi, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: oi          ! Ob Inc. structure.
   type (y_type), intent(in)    :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   real, intent(out)            :: jo_ssmi
   type (jo_type), intent(out)  :: jo          ! Obs cost function.

   jo_ssmi = 0.0
   jo % ssmir_speed = 0.0
   jo % ssmir_tpw = 0.0
   jo % ssmi_tb19v = 0.0
   jo % ssmi_tb19h = 0.0
   jo % ssmi_tb22v = 0.0
   jo % ssmi_tb37v = 0.0
   jo % ssmi_tb37h = 0.0
   jo % ssmi_tb85v = 0.0
   jo % ssmi_tb85h = 0.0

   if ( oi%num_ssmi_tb .gt. 0 ) then

      call da_cal_jo_and_grady_ssmi_tb( oi, re, jo, jo_grad_y )
      jo_ssmi = jo % ssmi_tb19v + jo % ssmi_tb19h + jo % ssmi_tb22v + &
                jo % ssmi_tb37v + jo % ssmi_tb37h + jo % ssmi_tb85v + &
                jo % ssmi_tb85h 
                
   else if ( oi%num_ssmi_retrieval .gt. 0) then

      call da_cal_jo_and_grady_ssmi_rv( oi, re, jo, jo_grad_y )
      jo_ssmi = jo % ssmir_speed + jo % ssmir_tpw

   end if

end subroutine da_cal_jo_and_grady_ssmi


