SUBROUTINE DA_Calculate_Jo_and_GradY_Buoy( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % buoy_u = 0.0
   jo % buoy_v = 0.0
   jo % buoy_t = 0.0
   jo % buoy_p = 0.0
   jo % buoy_q = 0.0

   IF ( iv%num_buoy > 0 ) THEN

      DO n=1, iv%num_buoy

       if( iv%buoy(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%buoy(n)%u = -re%buoy(n)%u / &
                                ( iv%buoy(n)%u%error * iv%buoy(n)%u%error)
         jo_grad_y%buoy(n)%v = -re%buoy(n)%v / &
                                ( iv%buoy(n)%v%error * iv%buoy(n)%v%error)
         jo_grad_y%buoy(n)%t = -re%buoy(n)%t / &
                                ( iv%buoy(n)%t%error * iv%buoy(n)%t%error)
         jo_grad_y%buoy(n)%p = -re%buoy(n)%p / &
                                ( iv%buoy(n)%p%error * iv%buoy(n)%p%error)
         jo_grad_y%buoy(n)%q = -re%buoy(n)%q / &
                                ( iv%buoy(n)%q%error * iv%buoy(n)%q%error)

       endif

       if( iv%buoy(n)%loc%proc_domain ) then

         jo % buoy_u = jo % buoy_u - re%buoy(n)%u * jo_grad_y%buoy(n)%u
         jo % buoy_v = jo % buoy_v - re%buoy(n)%v * jo_grad_y%buoy(n)%v
         jo % buoy_t = jo % buoy_t - re%buoy(n)%t * jo_grad_y%buoy(n)%t
         jo % buoy_p = jo % buoy_p - re%buoy(n)%p * jo_grad_y%buoy(n)%p
         jo % buoy_q = jo % buoy_q - re%buoy(n)%q * jo_grad_y%buoy(n)%q

       endif

      END DO
      
      jo % buoy_u = 0.5 * jo % buoy_u
      jo % buoy_v = 0.5 * jo % buoy_v
      jo % buoy_t = 0.5 * jo % buoy_t
      jo % buoy_p = 0.5 * jo % buoy_p
      jo % buoy_q = 0.5 * jo % buoy_q

   END IF
     
END SUBROUTINE DA_Calculate_Jo_and_GradY_Buoy



