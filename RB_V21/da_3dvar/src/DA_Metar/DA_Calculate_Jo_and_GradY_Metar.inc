SUBROUTINE DA_Calculate_Jo_and_GradY_metar( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), INTENT(INOUT) :: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % metar_u = 0.0
   jo % metar_v = 0.0
   jo % metar_t = 0.0
   jo % metar_p = 0.0
   jo % metar_q = 0.0

   IF ( iv%num_metar > 0 ) THEN

      DO n=1, iv%num_metar

       if( iv%metar(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%metar(n)%u = -re%metar(n)%u / &
                                ( iv%metar(n)%u%error * iv%metar(n)%u%error)
         jo_grad_y%metar(n)%v = -re%metar(n)%v / &
                                ( iv%metar(n)%v%error * iv%metar(n)%v%error)
         jo_grad_y%metar(n)%t = -re%metar(n)%t / &
                                ( iv%metar(n)%t%error * iv%metar(n)%t%error)
         jo_grad_y%metar(n)%p = -re%metar(n)%p / &
                                ( iv%metar(n)%p%error * iv%metar(n)%p%error)
         jo_grad_y%metar(n)%q = -re%metar(n)%q / &
                                ( iv%metar(n)%q%error * iv%metar(n)%q%error)

       endif

       if( iv%metar(n)%loc%proc_domain ) then

         jo % metar_u = jo % metar_u - re%metar(n)%u * jo_grad_y%metar(n)%u
         jo % metar_v = jo % metar_v - re%metar(n)%v * jo_grad_y%metar(n)%v
         jo % metar_t = jo % metar_t - re%metar(n)%t * jo_grad_y%metar(n)%t
         jo % metar_p = jo % metar_p - re%metar(n)%p * jo_grad_y%metar(n)%p
         jo % metar_q = jo % metar_q - re%metar(n)%q * jo_grad_y%metar(n)%q

       endif

      END DO
      
      jo % metar_u = 0.5 * jo % metar_u
      jo % metar_v = 0.5 * jo % metar_v
      jo % metar_t = 0.5 * jo % metar_t
      jo % metar_p = 0.5 * jo % metar_p
      jo % metar_q = 0.5 * jo % metar_q
      
   END IF
     
END SUBROUTINE DA_Calculate_Jo_and_GradY_metar

