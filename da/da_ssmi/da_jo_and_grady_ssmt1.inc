subroutine da_jo_and_grady_ssmt1.inc(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n, k

   jo % ssmt1_t = 0.0

   if (iv%num_ssmt1 > 0) then
   
      do n=1, iv%num_ssmt1
         do k=1, iv%ssmt1(n)%info%levels

            jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / &
                                      (iv%ssmt1(n)%t(k)%error * &
                                        iv%ssmt1(n)%t(k)%error)

            jo % ssmt1_t = jo % ssmt1_t - &
                           re%ssmt1(n)%t(k) * jo_grad_y%ssmt1(n)%t(k)
         end do
      end do

      jo % ssmt1_t = 0.5 * jo % ssmt1_t

   end if

end subroutine da_jo_and_grady_ssmt1.inc


