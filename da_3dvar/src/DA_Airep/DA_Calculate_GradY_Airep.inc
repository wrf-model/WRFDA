SUBROUTINE DA_Calculate_GradY_Airep( iv, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)


   INTEGER                      :: n, k

   IF ( iv%num_airep > 0 ) THEN

      DO n=1, iv%num_airep

       if( iv%airep(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%airep(n)%info%levels


        if(iv%airep(n)%u(k)%qc < obs_qc_pointer) re%airep(n)%u(k) = 0.0
        if(iv%airep(n)%v(k)%qc < obs_qc_pointer) re%airep(n)%v(k) = 0.0
        if(iv%airep(n)%t(k)%qc < obs_qc_pointer) re%airep(n)%t(k) = 0.0

        jo_grad_y%airep(n)%u(k) = -re%airep(n)%u(k) / &
                                      ( iv%airep(n)%u(k)%error * &
                                        iv%airep(n)%u(k)%error )
            jo_grad_y%airep(n)%v(k) = -re%airep(n)%v(k) / &
                                      ( iv%airep(n)%v(k)%error * &
                                        iv%airep(n)%v(k)%error )
            jo_grad_y%airep(n)%t(k) = -re%airep(n)%t(k) / &
                                      ( iv%airep(n)%t(k)%error * &
                                        iv%airep(n)%t(k)%error )
          end do

       endif

      END DO
      
   END IF

END SUBROUTINE DA_Calculate_GradY_Airep

