SUBROUTINE DA_Calculate_Jo_and_GradY_airep( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(OUT)  :: jo          ! Obs cost function.


   INTEGER                      :: n, k

   jo % airep_u = 0.0
   jo % airep_v = 0.0
   jo % airep_t = 0.0

   IF ( iv%num_airep > 0 ) THEN

      DO n=1, iv%num_airep

       if( iv%airep(n)%loc%proc_domain_with_halo ) then

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

       endif

       if( iv%airep(n)%loc%proc_domain ) then

         do k=1, iv%airep(n)%info%levels

            jo % airep_u = jo % airep_u - &
                         re%airep(n)%u(k) * jo_grad_y%airep(n)%u(k)
            jo % airep_v = jo % airep_v - &
                         re%airep(n)%v(k) * jo_grad_y%airep(n)%v(k)
            jo % airep_t = jo % airep_t - &
                         re%airep(n)%t(k) * jo_grad_y%airep(n)%t(k)
         end do

       endif

      END DO
      
      jo % airep_u = 0.5 * jo % airep_u
      jo % airep_v = 0.5 * jo % airep_v
      jo % airep_t = 0.5 * jo % airep_t
      
   END IF

END SUBROUTINE DA_Calculate_Jo_and_GradY_airep

