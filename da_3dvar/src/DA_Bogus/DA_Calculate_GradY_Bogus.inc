SUBROUTINE DA_Calculate_GradY_Bogus( iv, re, jo_grad_y )
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

   IF ( iv%num_bogus > 0 ) THEN
   
      DO n=1, iv%num_bogus

       if( iv%bogus(n)%loc%proc_domain_with_halo ) then

         if(iv%bogus(n)%slp%qc < obs_qc_pointer) re%bogus(n)%slp = 0.0

         jo_grad_y%bogus(n)%slp = -re%bogus(n)%slp / &
                                ( iv%bogus(n)%slp%error * iv%bogus(n)%slp%error)

         do k=1, iv%bogus(n)%info%levels

            if(iv%bogus(n)%u(k)%qc < obs_qc_pointer) re%bogus(n)%u(k) = 0.0
            if(iv%bogus(n)%v(k)%qc < obs_qc_pointer) re%bogus(n)%v(k) = 0.0
            if(iv%bogus(n)%t(k)%qc < obs_qc_pointer) re%bogus(n)%t(k) = 0.0
            if(iv%bogus(n)%q(k)%qc < obs_qc_pointer) re%bogus(n)%q(k) = 0.0

            jo_grad_y%bogus(n)%u(k) = -re%bogus(n)%u(k) / &
                                      ( iv%bogus(n)%u(k)%error * &
                                        iv%bogus(n)%u(k)%error )
            jo_grad_y%bogus(n)%v(k) = -re%bogus(n)%v(k) / &
                                      ( iv%bogus(n)%v(k)%error * &
                                        iv%bogus(n)%v(k)%error )
            jo_grad_y%bogus(n)%t(k) = -re%bogus(n)%t(k) / &
                                      ( iv%bogus(n)%t(k)%error * &
                                        iv%bogus(n)%t(k)%error )
            jo_grad_y%bogus(n)%q(k) = -re%bogus(n)%q(k) / &
                                      ( iv%bogus(n)%q(k)%error * &
                                        iv%bogus(n)%q(k)%error )
         end do
       
       endif

      END DO
      
   END IF

END SUBROUTINE DA_Calculate_GradY_Bogus
