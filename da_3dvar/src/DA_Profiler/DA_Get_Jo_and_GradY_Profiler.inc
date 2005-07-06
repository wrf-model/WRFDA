SUBROUTINE DA_Get_Jo_and_GradY_profiler( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), INTENT(INOUT) :: jo          ! Obs cost function.


   INTEGER                      :: n, k

   jo % profiler_u = 0.0
   jo % profiler_v = 0.0

   IF ( iv%num_profiler > 0 ) THEN
   
      DO n=1, iv%num_profiler

       if( iv%profiler(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%profiler(n)%info%levels

            jo_grad_y%profiler(n)%u(k) = -re%profiler(n)%u(k) / &
                                      ( iv%profiler(n)%u(k)%error * &
                                        iv%profiler(n)%u(k)%error )
            jo_grad_y%profiler(n)%v(k) = -re%profiler(n)%v(k) / &
                                      ( iv%profiler(n)%v(k)%error * &
                                        iv%profiler(n)%v(k)%error )
         end do
       
       endif

       if( iv%profiler(n)%loc%proc_domain ) then

         do k=1, iv%profiler(n)%info%levels

            jo % profiler_u = jo % profiler_u - &
                         re%profiler(n)%u(k) * jo_grad_y%profiler(n)%u(k)
            jo % profiler_v = jo % profiler_v - &
                         re%profiler(n)%v(k) * jo_grad_y%profiler(n)%v(k)
         end do

       endif

      END DO
      
      jo % profiler_u = 0.5 * jo % profiler_u
      jo % profiler_v = 0.5 * jo % profiler_v
      
   END IF

END SUBROUTINE DA_Get_Jo_and_GradY_profiler

