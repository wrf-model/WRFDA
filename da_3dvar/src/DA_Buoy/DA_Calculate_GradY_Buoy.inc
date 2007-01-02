SUBROUTINE DA_Calculate_GradY_Buoy( iv, re, jo_grad_y )
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
   TYPE (y_type), INTENT(INOUT)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n

   IF ( iv%num_buoy > 0 ) THEN

      DO n=1, iv%num_buoy

       if( iv%buoy(n)%loc%proc_domain_with_halo ) then

        if(iv%buoy(n)%u%qc < obs_qc_pointer) re%buoy(n)%u = 0.0
        if(iv%buoy(n)%v%qc < obs_qc_pointer) re%buoy(n)%v = 0.0
        if(iv%buoy(n)%t%qc < obs_qc_pointer) re%buoy(n)%t = 0.0
        if(iv%buoy(n)%p%qc < obs_qc_pointer) re%buoy(n)%p = 0.0
        if(iv%buoy(n)%q%qc < obs_qc_pointer) re%buoy(n)%q = 0.0

         jo_grad_y%buoy(n)%u = -re%buoy(n)%u / &
                                ( iv%buoy(n)%u%error * iv%buoy(n)%u%error)
         jo_grad_y%buoy(n)%v = -re%buoy(n)%v / &
                                ( iv%buoy(n)%v%error * iv%buoy(n)%v%error)
         jo_grad_y%buoy(n)%t = -re%buoy(n)%t / &
                                ( iv%buoy(n)%t%error * iv%buoy(n)%t%error)
         jo_grad_y%buoy(n)%p = -re%buoy(n)%p / &
                                ( iv%buoy(n)%p%error * iv%buoy(n)%p%error)
         jo_grad_y%buoy(n)%q = -re%buoy(n)%q / &
                                ( iv%buoy(n)%q%error * iv%buoy(n)%q%error)

       endif

      END DO

   END IF
     
END SUBROUTINE DA_Calculate_GradY_Buoy



