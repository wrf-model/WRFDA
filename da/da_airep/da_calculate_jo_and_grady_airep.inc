subroutine da_calculate_jo_and_grady_airep( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(out)   :: jo          ! Obs cost function.

   integer                      :: n, k

   jo % airep_u = 0.0
   jo % airep_v = 0.0
   jo % airep_t = 0.0

   if (iv%num_airep > 0) then
      do n=1, iv%num_airep
         do k=1, iv%airep(n)%info%levels
            jo_grad_y%airep(n)%u(k) = -re%airep(n)%u(k) / &
                                        ( iv%airep(n)%u(k)%error * &
                                          iv%airep(n)%u(k)%error )
            jo_grad_y%airep(n)%v(k) = -re%airep(n)%v(k) / &
                                        ( iv%airep(n)%v(k)%error * &
                                          iv%airep(n)%v(k)%error )
            jo_grad_y%airep(n)%t(k) = -re%airep(n)%t(k) / &
                                        ( iv%airep(n)%t(k)%error * &
                                          iv%airep(n)%t(k)%error )
          end do

          if (iv%airep(n)%loc%proc_domain) then
             do k=1, iv%airep(n)%info%levels
               jo % airep_u = jo % airep_u - &
                              re%airep(n)%u(k) * jo_grad_y%airep(n)%u(k)
               jo % airep_v = jo % airep_v - &
                              re%airep(n)%v(k) * jo_grad_y%airep(n)%v(k)
               jo % airep_t = jo % airep_t - &
                              re%airep(n)%t(k) * jo_grad_y%airep(n)%t(k)
             end do
         end if
      end do

      jo % airep_u = 0.5 * jo % airep_u
      jo % airep_v = 0.5 * jo % airep_v
      jo % airep_t = 0.5 * jo % airep_t

   end if

end subroutine da_calculate_jo_and_grady_airep

