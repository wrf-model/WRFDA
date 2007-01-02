SUBROUTINE DA_Calculate_Jo_and_GradY_Ships( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % ships_u = 0.0
   jo % ships_v = 0.0
   jo % ships_t = 0.0
   jo % ships_p = 0.0
   jo % ships_q = 0.0
   IF ( iv%num_ships > 0 ) THEN

      DO n=1, iv%num_ships

       if( iv%ships(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%ships(n)%u = -re%ships(n)%u / &
                                ( iv%ships(n)%u%error * iv%ships(n)%u%error)
         jo_grad_y%ships(n)%v = -re%ships(n)%v / &
                                ( iv%ships(n)%v%error * iv%ships(n)%v%error)
         jo_grad_y%ships(n)%t = -re%ships(n)%t / &
                                ( iv%ships(n)%t%error * iv%ships(n)%t%error)
         jo_grad_y%ships(n)%p = -re%ships(n)%p / &
                                ( iv%ships(n)%p%error * iv%ships(n)%p%error)
         jo_grad_y%ships(n)%q = -re%ships(n)%q / &
                                ( iv%ships(n)%q%error * iv%ships(n)%q%error)
       endif

       if( iv%ships(n)%loc%proc_domain ) then

         jo % ships_u = jo % ships_u - re%ships(n)%u * jo_grad_y%ships(n)%u
         jo % ships_v = jo % ships_v - re%ships(n)%v * jo_grad_y%ships(n)%v
         jo % ships_t = jo % ships_t - re%ships(n)%t * jo_grad_y%ships(n)%t
         jo % ships_p = jo % ships_p - re%ships(n)%p * jo_grad_y%ships(n)%p
         jo % ships_q = jo % ships_q - re%ships(n)%q * jo_grad_y%ships(n)%q

       endif

      END DO
      
      jo % ships_u = 0.5 * jo % ships_u
      jo % ships_v = 0.5 * jo % ships_v
      jo % ships_t = 0.5 * jo % ships_t
      jo % ships_p = 0.5 * jo % ships_p
      jo % ships_q = 0.5 * jo % ships_q
   END IF
     
END SUBROUTINE DA_Calculate_Jo_and_GradY_Ships



