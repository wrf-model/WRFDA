SUBROUTINE DA_Cal_Jo_and_GradY_SSMI_Rv( oi, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: oi          ! Ob Inc. structure.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual structure.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % ssmir_Speed=0.0
   jo % ssmir_tpw=0.0

   do n=1, oi%num_ssmi_retrieval

    if( oi%ssmi_retrieval(n)%loc%proc_domain_with_halo ) then

      jo_grad_y%ssmi_retrieval(n)%Speed = - re%ssmi_retrieval(n)%Speed / &
                                         ( oi%ssmi_retrieval(n)%Speed%error * &
                                           oi%ssmi_retrieval(n)%Speed%error )
                                           
      jo_grad_y%ssmi_retrieval(n)%tpw = -re%ssmi_retrieval(n)%tpw / &
                                       ( oi%ssmi_retrieval(n)%tpw%error * &
                                         oi%ssmi_retrieval(n)%tpw%error )
    endif

    if( oi%ssmi_retrieval(n)%loc%proc_domain ) then
    
      jo % ssmir_Speed = jo % ssmir_Speed - &
                      re%ssmi_retrieval(n)%Speed * &
                      jo_grad_y%ssmi_retrieval(n)%Speed

      jo % ssmir_tpw = jo % ssmir_tpw - &
                    re%ssmi_retrieval(n)%tpw * jo_grad_y%ssmi_retrieval(n)%tpw

    endif

   end do
   
   jo % ssmir_Speed = 0.5 * jo % ssmir_Speed
   jo % ssmir_tpw   = 0.5 * jo % ssmir_tpw

END SUBROUTINE DA_Cal_Jo_and_GradY_SSMI_Rv

