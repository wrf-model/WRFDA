SUBROUTINE DA_Calculate_Jo_and_GradY_satob( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % satob_u = 0.0
   jo % satob_v = 0.0

   IF ( iv%num_satob > 0 ) THEN

      DO n=1, iv%num_satob

       if( iv%satob(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%satob(n)%u = -re%satob(n)%u / &
                                ( iv%satob(n)%u%error * iv%satob(n)%u%error)
         jo_grad_y%satob(n)%v = -re%satob(n)%v / &
                                ( iv%satob(n)%v%error * iv%satob(n)%v%error)
       endif

       if( iv%satob(n)%loc%proc_domain ) then

         jo % satob_u = jo % satob_u - re%satob(n)%u * jo_grad_y%satob(n)%u
         jo % satob_v = jo % satob_v - re%satob(n)%v * jo_grad_y%satob(n)%v

       endif

      END DO
      
      jo % satob_u = 0.5 * jo % satob_u
      jo % satob_v = 0.5 * jo % satob_v
      
   END IF
     
END SUBROUTINE DA_Calculate_Jo_and_GradY_satob

