subroutine da_cal_jo_and_grady_pseudo( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n

   jo % pseudo_u = 0.0
   jo % pseudo_v = 0.0
   jo % pseudo_t = 0.0
   jo % pseudo_p = 0.0
   jo % pseudo_q = 0.0

   if ( iv%num_pseudo > 0 ) then
      do n=1, iv%num_pseudo

         jo_grad_y%pseudo(n)%u = -re%pseudo(n)%u / &
                                ( iv%pseudo(n)%u%error * iv%pseudo(n)%u%error)
         jo_grad_y%pseudo(n)%v = -re%pseudo(n)%v / &
                                ( iv%pseudo(n)%v%error * iv%pseudo(n)%v%error)
         jo_grad_y%pseudo(n)%t = -re%pseudo(n)%t / &
                                ( iv%pseudo(n)%t%error * iv%pseudo(n)%t%error)
         jo_grad_y%pseudo(n)%p = -re%pseudo(n)%p / &
                                ( iv%pseudo(n)%p%error * iv%pseudo(n)%p%error)
         jo_grad_y%pseudo(n)%q = -re%pseudo(n)%q / &
                                ( iv%pseudo(n)%q%error * iv%pseudo(n)%q%error)

         if (iv%pseudo(n)%loc%proc_domain) then
            jo % pseudo_u = jo % pseudo_u - re%pseudo(n)%u * jo_grad_y%pseudo(n)%u
            jo % pseudo_v = jo % pseudo_v - re%pseudo(n)%v * jo_grad_y%pseudo(n)%v
            jo % pseudo_t = jo % pseudo_t - re%pseudo(n)%t * jo_grad_y%pseudo(n)%t
            jo % pseudo_p = jo % pseudo_p - re%pseudo(n)%p * jo_grad_y%pseudo(n)%p
            jo % pseudo_q = jo % pseudo_q - re%pseudo(n)%q * jo_grad_y%pseudo(n)%q
         end if
      end do
      
      jo % pseudo_u = 0.5 * jo % pseudo_u
      jo % pseudo_v = 0.5 * jo % pseudo_v
      jo % pseudo_t = 0.5 * jo % pseudo_t
      jo % pseudo_p = 0.5 * jo % pseudo_p
      jo % pseudo_q = 0.5 * jo % pseudo_q      
      
   end if
  
end subroutine da_cal_jo_and_grady_pseudo


