SUBROUTINE DA_Calculate_GradY_Satob( iv, re, jo_grad_y )
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

   INTEGER                      :: n


   IF ( iv%num_satob > 0 ) THEN

      DO n=1, iv%num_satob

       if( iv%satob(n)%loc%proc_domain_with_halo ) then

        if(iv%satob(n)%u%qc < obs_qc_pointer) re%satob(n)%u = 0.0
        if(iv%satob(n)%v%qc < obs_qc_pointer) re%satob(n)%v = 0.0

         jo_grad_y%satob(n)%u = -re%satob(n)%u / &
                                ( iv%satob(n)%u%error * iv%satob(n)%u%error)
         jo_grad_y%satob(n)%v = -re%satob(n)%v / &
                                ( iv%satob(n)%v%error * iv%satob(n)%v%error)
       endif

      END DO
      
   END IF
     
END SUBROUTINE DA_Calculate_GradY_Satob



