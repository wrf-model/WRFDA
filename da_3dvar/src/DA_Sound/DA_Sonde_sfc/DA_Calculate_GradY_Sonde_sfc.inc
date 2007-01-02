SUBROUTINE DA_Calculate_GradY_Sonde_sfc( iv, re, jo_grad_y )
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


   IF ( iv%num_sound < 1 ) return

      DO n=1, iv%num_sound

       if( iv%sonde_sfc(n)%loc%proc_domain_with_halo ) then

        if(iv%sonde_sfc(n)%u%qc < obs_qc_pointer) re%sonde_sfc(n)%u = 0.0
        if(iv%sonde_sfc(n)%v%qc < obs_qc_pointer) re%sonde_sfc(n)%v = 0.0
        if(iv%sonde_sfc(n)%t%qc < obs_qc_pointer) re%sonde_sfc(n)%t = 0.0
        if(iv%sonde_sfc(n)%p%qc < obs_qc_pointer) re%sonde_sfc(n)%p = 0.0
        if(iv%sonde_sfc(n)%q%qc < obs_qc_pointer) re%sonde_sfc(n)%q = 0.0

        if(iv%sonde_sfc(n)%u%qc < obs_qc_pointer) re%sonde_sfc(n)%u = 0.0
        if(iv%sonde_sfc(n)%v%qc < obs_qc_pointer) re%sonde_sfc(n)%v = 0.0
        if(iv%sonde_sfc(n)%t%qc < obs_qc_pointer) re%sonde_sfc(n)%t = 0.0
        if(iv%sonde_sfc(n)%p%qc < obs_qc_pointer) re%sonde_sfc(n)%p = 0.0
        if(iv%sonde_sfc(n)%q%qc < obs_qc_pointer) re%sonde_sfc(n)%q = 0.0

         jo_grad_y%sonde_sfc(n)%u = -re%sonde_sfc(n)%u / &
                                ( iv%sonde_sfc(n)%u%error * iv%sonde_sfc(n)%u%error)
         jo_grad_y%sonde_sfc(n)%v = -re%sonde_sfc(n)%v / &
                                ( iv%sonde_sfc(n)%v%error * iv%sonde_sfc(n)%v%error)
         jo_grad_y%sonde_sfc(n)%t = -re%sonde_sfc(n)%t / &
                                ( iv%sonde_sfc(n)%t%error * iv%sonde_sfc(n)%t%error)
         jo_grad_y%sonde_sfc(n)%p = -re%sonde_sfc(n)%p / &
                                ( iv%sonde_sfc(n)%p%error * iv%sonde_sfc(n)%p%error)
         jo_grad_y%sonde_sfc(n)%q = -re%sonde_sfc(n)%q / &
                                ( iv%sonde_sfc(n)%q%error * iv%sonde_sfc(n)%q%error)
       endif

      END DO
      
     
END SUBROUTINE DA_Calculate_GradY_Sonde_sfc



