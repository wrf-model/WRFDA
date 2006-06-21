subroutine da_cal_jo_and_grady_ssmi( oi, re, jo, jo_ssmi, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: oi          ! Ob Inc. structure.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual structure.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   real, intent(out)            :: jo_ssmi
   type (jo_type), INTENT(OUT)  :: jo          ! Obs cost function.

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

   IF ( oi%num_ssmi_tb .gt. 0 ) THEN

      call da_cal_jo_and_grady_ssmi_tb( oi, re, jo, jo_grad_y )
      jo_ssmi = jo % ssmi_tb19v + jo % ssmi_tb19h + jo % ssmi_tb22v + &
                jo % ssmi_tb37v + jo % ssmi_tb37h + jo % ssmi_tb85v + &
                jo % ssmi_tb85h 
                
   ELSE IF ( oi%num_ssmi_retrieval .gt. 0) THEN

      call da_cal_jo_and_grady_ssmi_rv( oi, re, jo, jo_grad_y )
      jo_ssmi = jo % ssmir_speed + jo % ssmir_tpw

   ENDIF

end subroutine da_cal_jo_and_grady_ssmi
