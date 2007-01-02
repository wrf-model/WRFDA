SUBROUTINE DA_Calculate_GradY_Gpspw( iv, re, jo_grad_y )
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


   IF ( iv%num_Gpspw > 0 ) THEN
   
      DO n=1, iv%num_Gpspw

       if( iv%gpspw(n)%loc%proc_domain_with_halo ) then

         if(iv%gpspw(n)%tpw%qc < obs_qc_pointer) &
            re%gpspw(n)%tpw = 0.0

         jo_grad_y%Gpspw(n)%tpw = -re%Gpspw(n)%tpw / &
                                  ( iv%Gpspw(n)%tpw%error * &
                                    iv%Gpspw(n)%tpw%error)
       endif

      END DO

   END IF

END SUBROUTINE DA_Calculate_GradY_Gpspw

