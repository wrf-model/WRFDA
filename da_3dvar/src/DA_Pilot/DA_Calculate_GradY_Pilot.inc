SUBROUTINE DA_Calculate_GradY_Pilot( iv, re, jo_grad_y )
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


   IF ( iv%num_pilot > 0 ) THEN
   
      DO n=1, iv%num_pilot

       if( iv%pilot(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%pilot(n)%info%levels

        if(iv%pilot(n)%u(k)%qc < obs_qc_pointer) re%pilot(n)%u(k) = 0.0
        if(iv%pilot(n)%v(k)%qc < obs_qc_pointer) re%pilot(n)%v(k) = 0.0

            jo_grad_y%pilot(n)%u(k) = -re%pilot(n)%u(k) / &
                                      ( iv%pilot(n)%u(k)%error * &
                                        iv%pilot(n)%u(k)%error )
            jo_grad_y%pilot(n)%v(k) = -re%pilot(n)%v(k) / &
                                      ( iv%pilot(n)%v(k)%error * &
                                        iv%pilot(n)%v(k)%error )
         end do
       
       endif

      END DO
      
      
   END IF

END SUBROUTINE DA_Calculate_GradY_Pilot

