subroutine da_jo_and_grady_ssmt2(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   jo % ssmt2_rh = 0.0
   
   do n=1, iv%num_ssmt2
      do k=1, iv%ssmt2(n)%info%levels
         jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
            (iv%ssmt2(n)%rh(k)%error * iv%ssmt2(n)%rh(k)%error)
         jo % ssmt2_rh = jo % ssmt2_rh - &
            re%ssmt2(n)%rh(k) * jo_grad_y%ssmt2(n)%rh(k)
      end do
   end do

   jo % ssmt2_rh = 0.5 * jo % ssmt2_rh

end subroutine da_jo_and_grady_ssmt2


