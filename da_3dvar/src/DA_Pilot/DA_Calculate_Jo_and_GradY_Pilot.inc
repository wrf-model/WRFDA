SUBROUTINE DA_Calculate_Jo_and_GradY_pilot( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), INTENT(INOUT) :: jo          ! Obs cost function.


   INTEGER                      :: n, k

   jo % pilot_u = 0.0
   jo % pilot_v = 0.0

   IF ( iv%num_pilot > 0 ) THEN
   
      DO n=1, iv%num_pilot

       if( iv%pilot(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%pilot(n)%info%levels

            jo_grad_y%pilot(n)%u(k) = -re%pilot(n)%u(k) / &
                                      ( iv%pilot(n)%u(k)%error * &
                                        iv%pilot(n)%u(k)%error )
            jo_grad_y%pilot(n)%v(k) = -re%pilot(n)%v(k) / &
                                      ( iv%pilot(n)%v(k)%error * &
                                        iv%pilot(n)%v(k)%error )
         end do
       
       endif

       if( iv%pilot(n)%loc%proc_domain ) then

         do k=1, iv%pilot(n)%info%levels

            jo % pilot_u = jo % pilot_u - &
                         re%pilot(n)%u(k) * jo_grad_y%pilot(n)%u(k)
            jo % pilot_v = jo % pilot_v - &
                         re%pilot(n)%v(k) * jo_grad_y%pilot(n)%v(k)
         end do

       endif

      END DO
      
      jo % pilot_u = 0.5 * jo % pilot_u
      jo % pilot_v = 0.5 * jo % pilot_v
      
   END IF

END SUBROUTINE DA_Calculate_Jo_and_GradY_pilot

