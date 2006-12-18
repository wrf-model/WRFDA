SUBROUTINE DA_Calculate_Jo_and_GradY_Satem( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % satem_thickness = 0.0

   IF ( iv%num_Satem > 0 ) THEN
   
      DO n=1, iv%num_Satem

       if( iv%satem(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%satem(n)%info%levels

            jo_grad_y%Satem(n)%thickness(k) = -re%Satem(n)%thickness(k) / &
                                      ( iv%Satem(n)%thickness(k)%error * &
                                        iv%Satem(n)%thickness(k)%error )
         end do

       endif

       if( iv%satem(n)%loc%proc_domain ) then

         do k=1, iv%satem(n)%info%levels

            jo % satem_thickness = jo % satem_thickness - &
                   re%Satem(n)%thickness(k) * jo_grad_y%Satem(n)%thickness(k)
         end do

       endif

      END DO
      
      jo % satem_thickness = 0.5 * jo % satem_thickness
      
   END IF

END SUBROUTINE DA_Calculate_Jo_and_GradY_Satem

