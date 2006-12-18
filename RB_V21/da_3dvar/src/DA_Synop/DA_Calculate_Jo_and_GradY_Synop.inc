SUBROUTINE DA_Calculate_Jo_and_GradY_Synop( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % synop_u = 0.0
   jo % synop_v = 0.0
   jo % synop_t = 0.0
   jo % synop_p = 0.0
   jo % synop_q = 0.0

   IF ( iv%num_synop > 0 ) THEN

      DO n=1, iv%num_synop

       if( iv%synop(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%synop(n)%u = -re%synop(n)%u / &
                                ( iv%synop(n)%u%error * iv%synop(n)%u%error)
         jo_grad_y%synop(n)%v = -re%synop(n)%v / &
                                ( iv%synop(n)%v%error * iv%synop(n)%v%error)
         jo_grad_y%synop(n)%t = -re%synop(n)%t / &
                                ( iv%synop(n)%t%error * iv%synop(n)%t%error)
         jo_grad_y%synop(n)%p = -re%synop(n)%p / &
                                ( iv%synop(n)%p%error * iv%synop(n)%p%error)
         jo_grad_y%synop(n)%q = -re%synop(n)%q / &
                                ( iv%synop(n)%q%error * iv%synop(n)%q%error)
       endif
  
       if( iv%synop(n)%loc%proc_domain ) then

         jo % synop_u = jo % synop_u - re%synop(n)%u * jo_grad_y%synop(n)%u
         jo % synop_v = jo % synop_v - re%synop(n)%v * jo_grad_y%synop(n)%v
         jo % synop_t = jo % synop_t - re%synop(n)%t * jo_grad_y%synop(n)%t
         jo % synop_p = jo % synop_p - re%synop(n)%p * jo_grad_y%synop(n)%p
         jo % synop_q = jo % synop_q - re%synop(n)%q * jo_grad_y%synop(n)%q

       endif

      END DO
      
      jo % synop_u = 0.5 * jo % synop_u
      jo % synop_v = 0.5 * jo % synop_v
      jo % synop_t = 0.5 * jo % synop_t
      jo % synop_p = 0.5 * jo % synop_p
      jo % synop_q = 0.5 * jo % synop_q

   END IF
     
END SUBROUTINE DA_Calculate_Jo_and_GradY_Synop



