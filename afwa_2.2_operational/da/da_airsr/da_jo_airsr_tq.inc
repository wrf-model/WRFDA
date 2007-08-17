subroutine da_jo_airsr_tq(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in  ) :: iv         ! Innovation vector.
   type (y_type),  intent(in  ) :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.
   ! Local declarations
   integer :: n, k
   do n=1, iv%num_airsr
      if (iv%airsr(n)%loc%proc_domain) then
         do k=1, iv%airsr(n)%info%levels
            jo % airsr_t = jo % airsr_t - &
                       re%airsr(n)%t(k) * jo_grad_y%airsr(n)%t(k)
            jo % airsr_q = jo % airsr_q - &
                       re%airsr(n)%q(k) * jo_grad_y%airsr(n)%q(k)
         end do
      end if
   end do

end subroutine da_jo_airsr_tq


