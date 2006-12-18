SUBROUTINE DA_Calculate_GradY_SSMT1( iv, re, jo_grad_y )
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

   INTEGER                      :: n, k


   IF ( iv%num_ssmt1 > 0 ) THEN
   
      DO n=1, iv%num_ssmt1
         do k=1, iv%ssmt1(n)%info%levels

        if(iv%ssmt1(n)%t(k)%qc < obs_qc_pointer) re%ssmt1(n)%t(k) = 0.0
            jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / &
                                      ( iv%ssmt1(n)%t(k)%error * &
                                        iv%ssmt1(n)%t(k)%error )

         end do
      END DO

   END IF

END SUBROUTINE DA_Calculate_GradY_SSMT1

