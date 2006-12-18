SUBROUTINE DA_Calculate_GradY_Synop( iv, re, jo_grad_y )
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


   IF ( iv%num_synop > 0 ) THEN

      DO n=1, iv%num_synop

       if( iv%synop(n)%loc%proc_domain_with_halo ) then

        if(iv%synop(n)%u%qc < obs_qc_pointer) re%synop(n)%u = 0.0
        if(iv%synop(n)%v%qc < obs_qc_pointer) re%synop(n)%v = 0.0
        if(iv%synop(n)%t%qc < obs_qc_pointer) re%synop(n)%t = 0.0
        if(iv%synop(n)%p%qc < obs_qc_pointer) re%synop(n)%p = 0.0
        if(iv%synop(n)%q%qc < obs_qc_pointer) re%synop(n)%q = 0.0

        if(iv%synop(n)%u%qc < obs_qc_pointer) re%synop(n)%u = 0.0
        if(iv%synop(n)%v%qc < obs_qc_pointer) re%synop(n)%v = 0.0
        if(iv%synop(n)%t%qc < obs_qc_pointer) re%synop(n)%t = 0.0
        if(iv%synop(n)%p%qc < obs_qc_pointer) re%synop(n)%p = 0.0
        if(iv%synop(n)%q%qc < obs_qc_pointer) re%synop(n)%q = 0.0

         jo_grad_y%synop(n)%u = -re%synop(n)%u / &
                                ( iv%synop(n)%u%error * iv%synop(n)%u%error)
         jo_grad_y%synop(n)%v = -re%synop(n)%v / &
                                ( iv%synop(n)%v%error * iv%synop(n)%v%error)
         jo_grad_y%synop(n)%t = -re%synop(n)%t / &
                                ( iv%synop(n)%t%error * iv%synop(n)%t%error)
         jo_grad_y%synop(n)%p = -re%synop(n)%p / &
                                ( iv%synop(n)%p%error * iv%synop(n)%p%error)
         jo_grad_y%synop(n)%q = -re%synop(n)%q / &
                                ( iv%synop(n)%q%error * iv%synop(n)%q%error)
       endif

      END DO
      

   END IF
     
END SUBROUTINE DA_Calculate_GradY_Synop



