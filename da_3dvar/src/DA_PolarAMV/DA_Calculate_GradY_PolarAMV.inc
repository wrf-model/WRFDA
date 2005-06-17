SUBROUTINE DA_Calculate_GradY_PolarAMV( iv, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 03/05/2005                   Syed RH Rizvi   
!
!      Purpose: Calculates Gradient of Polar AMVs  Obs.
!               
!-------------------------------------------------------------------------
   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n , k

   IF ( iv%num_polaramv > 0 ) THEN

      DO n=1, iv%num_polaramv

       if( iv%polaramv(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%polaramv(n)%info%levels

        if(iv%polaramv(n)%u(k)%qc < obs_qc_pointer) re%polaramv(n)%u(k) = 0.0
        if(iv%polaramv(n)%v(k)%qc < obs_qc_pointer) re%polaramv(n)%v(k) = 0.0

            jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
                                      ( iv%polaramv(n)%u(k)%error * &
                                        iv%polaramv(n)%u(k)%error )
            jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
                                      ( iv%polaramv(n)%v(k)%error * &
                                        iv%polaramv(n)%v(k)%error )
         end do

       endif

      END DO


   END IF

END SUBROUTINE DA_Calculate_GradY_PolarAMV
