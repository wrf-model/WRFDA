SUBROUTINE DA_Calculate_Jo_and_GradY_Bogus( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo%Bogus_u   = 0.0
   jo%Bogus_v   = 0.0
   jo%Bogus_t   = 0.0
   jo%Bogus_q   = 0.0
   jo%Bogus_slp = 0.0

   IF ( iv%num_Bogus > 0 ) THEN
   
      DO n=1, iv%num_Bogus

       if( iv%bogus(n)%loc%proc_domain_with_halo ) then

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

       end if

       if( iv%bogus(n)%loc%proc_domain ) then

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
         enddo
     
       end if 

      END DO

      jo % bogus_slp = 0.5 * jo % bogus_slp
      jo % bogus_u = 0.5 * jo % bogus_u
      jo % bogus_v = 0.5 * jo % bogus_v
      jo % bogus_t = 0.5 * jo % bogus_t
      jo % bogus_q = 0.5 * jo % bogus_q

   END IF

END SUBROUTINE DA_Calculate_Jo_and_GradY_Bogus

