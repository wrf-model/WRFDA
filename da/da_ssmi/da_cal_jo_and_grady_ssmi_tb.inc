subroutine da_cal_jo_and_grady_ssmi_tb(oi, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: oi          ! Ob Inc. structure.
   type (y_type), intent(in)    :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(out)  :: jo          ! Obs cost function.

   integer                      :: n

   jo % ssmi_tb19v = 0.
   jo % ssmi_tb19h = 0.
   jo % ssmi_tb22v = 0.
   jo % ssmi_tb37v = 0.
   jo % ssmi_tb37h = 0.
   jo % ssmi_tb85v = 0.
   jo % ssmi_tb85h = 0.

   do n=1, oi%num_ssmi_tb

      jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / &
                                    (oi%ssmi_tb(n)%tb19v%error * &
                                      oi%ssmi_tb(n)%tb19v%error)

      jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / &
                                    (oi%ssmi_tb(n)%tb19h%error * &
                                      oi%ssmi_tb(n)%tb19h%error)

      jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / &
                                    (oi%ssmi_tb(n)%tb22v%error * &
                                      oi%ssmi_tb(n)%tb22v%error)

      jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / &
                                    (oi%ssmi_tb(n)%tb37v%error * &
                                      oi%ssmi_tb(n)%tb37v%error)

      jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / &
                                    (oi%ssmi_tb(n)%tb37h%error * &
                                      oi%ssmi_tb(n)%tb37h%error)

      jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / &
                                    (oi%ssmi_tb(n)%tb85v%error * &
                                      oi%ssmi_tb(n)%tb85v%error)

      jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / &
                                    (oi%ssmi_tb(n)%tb85h%error * &
                                      oi%ssmi_tb(n)%tb85h%error)

      if (oi%ssmi_tb(n)%loc%proc_domain) then
         jo % ssmi_tb19v = jo % ssmi_tb19v - &
                         re%ssmi_tb(n)%tb19v * jo_grad_y%ssmi_tb(n)%tb19v 

         jo % ssmi_tb19h = jo % ssmi_tb19h - &
                         re%ssmi_tb(n)%tb19h * jo_grad_y%ssmi_tb(n)%tb19h

         jo % ssmi_tb22v = jo % ssmi_tb22v - &
                         re%ssmi_tb(n)%tb22v * jo_grad_y%ssmi_tb(n)%tb22v

         jo % ssmi_tb37v = jo % ssmi_tb37v - &
                         re%ssmi_tb(n)%tb37v * jo_grad_y%ssmi_tb(n)%tb37v

         jo % ssmi_tb37h = jo % ssmi_tb37h - &
                         re%ssmi_tb(n)%tb37h * jo_grad_y%ssmi_tb(n)%tb37h

         jo % ssmi_tb85v = jo % ssmi_tb85v - &
                         re%ssmi_tb(n)%tb85v * jo_grad_y%ssmi_tb(n)%tb85v

         jo % ssmi_tb85h = jo % ssmi_tb85h - &
                         re%ssmi_tb(n)%tb85h * jo_grad_y%ssmi_tb(n)%tb85h

      end if
   end do
   
   jo % ssmi_tb19h = 0.5 * jo % ssmi_tb19h
   jo % ssmi_tb19v = 0.5 * jo % ssmi_tb19v
   jo % ssmi_tb22v = 0.5 * jo % ssmi_tb22v
   jo % ssmi_tb37h = 0.5 * jo % ssmi_tb37h
   jo % ssmi_tb37v = 0.5 * jo % ssmi_tb37v
   jo % ssmi_tb85h = 0.5 * jo % ssmi_tb85h
   jo % ssmi_tb85v = 0.5 * jo % ssmi_tb85v

end subroutine da_cal_jo_and_grady_ssmi_tb


