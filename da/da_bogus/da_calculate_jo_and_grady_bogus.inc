subroutine da_calculate_jo_and_grady_bogus( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n, k

   jo%Bogus_u   = 0.0
   jo%Bogus_v   = 0.0
   jo%Bogus_t   = 0.0
   jo%Bogus_q   = 0.0
   jo%Bogus_slp = 0.0

   if (iv%num_Bogus > 0) then
      do n=1, iv%num_Bogus
         jo_grad_y%Bogus(n)%slp = -re%Bogus(n)%slp /  &
                                  ( iv%Bogus(n)%slp%error * &
                                    iv%Bogus(n)%slp%error )
         do k=1, iv%bogus(n)%info%levels
            jo_grad_y%Bogus(n)%u(k) = -re%Bogus(n)%u(k) / &
                                       ( iv%Bogus(n)%u(k)%error * &
                                         iv%Bogus(n)%u(k)%error )
            jo_grad_y%Bogus(n)%v(k) = -re%Bogus(n)%v(k) / &
                                       ( iv%Bogus(n)%v(k)%error * &
                                         iv%Bogus(n)%v(k)%error )
            jo_grad_y%Bogus(n)%t(k) = -re%Bogus(n)%t(k) / &
                                       ( iv%Bogus(n)%t(k)%error * &
                                         iv%Bogus(n)%t(k)%error )
            jo_grad_y%Bogus(n)%q(k) = -re%Bogus(n)%q(k) / &
                                       ( iv%Bogus(n)%q(k)%error * &
                                         iv%Bogus(n)%q(k)%error )
         end do

         if (iv%bogus(n)%loc%proc_domain) then

            jo%Bogus_slp = jo%Bogus_slp -   &
                           re%Bogus(n)%slp * jo_grad_y%Bogus(n)%slp

            do k=1, iv%bogus(n)%info%levels
               jo%Bogus_u = jo%Bogus_u - &
                             re%Bogus(n)%u(k) * jo_grad_y%Bogus(n)%u(k)
               jo%Bogus_v = jo%Bogus_v - &
                             re%Bogus(n)%v(k) * jo_grad_y%Bogus(n)%v(k)
               jo%Bogus_t = jo%Bogus_t - &
                             re%Bogus(n)%t(k) * jo_grad_y%Bogus(n)%t(k)
               jo%Bogus_q = jo%Bogus_q - &
                             re%Bogus(n)%q(k) * jo_grad_y%Bogus(n)%q(k)
            end do
         end if 
      end do

      jo % bogus_slp = 0.5 * jo % bogus_slp
      jo % bogus_u = 0.5 * jo % bogus_u
      jo % bogus_v = 0.5 * jo % bogus_v
      jo % bogus_t = 0.5 * jo % bogus_t
      jo % bogus_q = 0.5 * jo % bogus_q

   end if

end subroutine da_calculate_jo_and_grady_bogus

