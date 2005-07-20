SUBROUTINE DA_Get_Jo_and_GradY_PolarAMV( iv, re, jo, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose:  Calculates Cost function and Gradient for Polar AMVs Obs
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % polaramv_u = 0.0
   jo % polaramv_v = 0.0

   IF ( iv%num_polaramv > 0 ) THEN

      DO n=1, iv%num_polaramv

       if( iv%polaramv(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%polaramv(n)%info%levels

            jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
                                      ( iv%polaramv(n)%u(k)%error * &
                                        iv%polaramv(n)%u(k)%error )
            jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
                                      ( iv%polaramv(n)%v(k)%error * &
                                        iv%polaramv(n)%v(k)%error )
         end do

       endif

       if( iv%polaramv(n)%loc%proc_domain ) then

         do k=1, iv%polaramv(n)%info%levels

            jo % polaramv_u = jo % polaramv_u - &
                         re%polaramv(n)%u(k) * jo_grad_y%polaramv(n)%u(k)
            jo % polaramv_v = jo % polaramv_v - &
                         re%polaramv(n)%v(k) * jo_grad_y%polaramv(n)%v(k)
         end do

       endif

      END DO

      jo % polaramv_u = 0.5 * jo % polaramv_u
      jo % polaramv_v = 0.5 * jo % polaramv_v

   END IF

     
END SUBROUTINE DA_Get_Jo_and_GradY_PolarAMV

