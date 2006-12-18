subroutine DA_Calculate_GradY_SSMT2( iv, re, jo_grad_y )
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

   IF ( iv%num_ssmt2 > 0 ) THEN
   
      DO n=1, iv%num_ssmt2
         do k=1, iv%ssmt2(n)%info%levels
        if(iv%ssmt2(n)%rh(k)%qc < obs_qc_pointer) re%ssmt2(n)%rh(k) = 0.0

            jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
                                      ( iv%ssmt2(n)%rh(k)%error * &
                                        iv%ssmt2(n)%rh(k)%error )

         end do
      END DO

   END IF

END subroutine DA_Calculate_GradY_SSMT2

