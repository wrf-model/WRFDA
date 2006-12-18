SUBROUTINE DA_Calculate_GradY_Ships( iv, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(INOUT) :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n

   IF ( iv%num_ships > 0 ) THEN

      DO n=1, iv%num_ships

       if( iv%ships(n)%loc%proc_domain_with_halo ) then

        if(iv%ships(n)%u%qc < obs_qc_pointer) re%ships(n)%u = 0.0
        if(iv%ships(n)%v%qc < obs_qc_pointer) re%ships(n)%v = 0.0
        if(iv%ships(n)%t%qc < obs_qc_pointer) re%ships(n)%t = 0.0
        if(iv%ships(n)%p%qc < obs_qc_pointer) re%ships(n)%p = 0.0
        if(iv%ships(n)%q%qc < obs_qc_pointer) re%ships(n)%q = 0.0

         jo_grad_y%ships(n)%u = -re%ships(n)%u / &
                                ( iv%ships(n)%u%error * iv%ships(n)%u%error)
         jo_grad_y%ships(n)%v = -re%ships(n)%v / &
                                ( iv%ships(n)%v%error * iv%ships(n)%v%error)
         jo_grad_y%ships(n)%t = -re%ships(n)%t / &
                                ( iv%ships(n)%t%error * iv%ships(n)%t%error)
         jo_grad_y%ships(n)%p = -re%ships(n)%p / &
                                ( iv%ships(n)%p%error * iv%ships(n)%p%error)
         jo_grad_y%ships(n)%q = -re%ships(n)%q / &
                                ( iv%ships(n)%q%error * iv%ships(n)%q%error)

       endif

      END DO
      
   END IF
     
END SUBROUTINE DA_Calculate_GradY_Ships



