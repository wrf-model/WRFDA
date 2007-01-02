SUBROUTINE DA_Calculate_GradY_Sound( iv, re, jo_grad_y )
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

   IF ( iv%num_sound > 0 ) THEN
   
      DO n=1, iv%num_sound
       if( iv%sound(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%sound(n)%info%levels

            if(iv%sound(n)%u(k)%qc < obs_qc_pointer) re%sound(n)%u(k) = 0.0
            if(iv%sound(n)%v(k)%qc < obs_qc_pointer) re%sound(n)%v(k) = 0.0
            if(iv%sound(n)%t(k)%qc < obs_qc_pointer) re%sound(n)%t(k) = 0.0
            if(iv%sound(n)%q(k)%qc < obs_qc_pointer) re%sound(n)%q(k) = 0.0

            jo_grad_y%sound(n)%u(k) = -re%sound(n)%u(k) / &
                                      ( iv%sound(n)%u(k)%error * &
                                        iv%sound(n)%u(k)%error )
            jo_grad_y%sound(n)%v(k) = -re%sound(n)%v(k) / &
                                      ( iv%sound(n)%v(k)%error * &
                                        iv%sound(n)%v(k)%error )
            jo_grad_y%sound(n)%t(k) = -re%sound(n)%t(k) / &
                                      ( iv%sound(n)%t(k)%error * &
                                        iv%sound(n)%t(k)%error )
            jo_grad_y%sound(n)%q(k) = -re%sound(n)%q(k) / &
                                      ( iv%sound(n)%q(k)%error * &
                                        iv%sound(n)%q(k)%error )
          end do
       endif  
      END DO
   END IF

END SUBROUTINE DA_Calculate_GradY_Sound
