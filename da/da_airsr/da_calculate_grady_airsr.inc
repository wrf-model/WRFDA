subroutine da_calculate_grady_airsr( iv, re, jo_grad_y )

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on AIRS re-vector             
   !----------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n, k

   IF ( iv%num_airsr > 0 ) THEN
      DO n=1, iv%num_airsr
         do k=1, iv%airsr(n)%info%levels
            if(iv%airsr(n)%t(k)%qc < obs_qc_pointer) re%airsr(n)%t(k) = 0.0
            if(iv%airsr(n)%q(k)%qc < obs_qc_pointer) re%airsr(n)%q(k) = 0.0

            jo_grad_y%airsr(n)%t(k) = -re%airsr(n)%t(k) / &
                                     ( iv%airsr(n)%t(k)%error * &
                                       iv%airsr(n)%t(k)%error )
            jo_grad_y%airsr(n)%q(k) = -re%airsr(n)%q(k) / &
                                     ( iv%airsr(n)%q(k)%error * &
                                       iv%airsr(n)%q(k)%error )
          end do
       END DO
   END IF

end subroutine da_calculate_grady_airsr

