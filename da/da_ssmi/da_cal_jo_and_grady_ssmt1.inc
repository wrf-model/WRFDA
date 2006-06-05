SUBROUTINE da_cal_jo_and_grady_ssmt1( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % ssmt1_t = 0.0

   IF ( iv%num_ssmt1 > 0 ) THEN
   
      DO n=1, iv%num_ssmt1
         do k=1, iv%ssmt1(n)%info%levels

            jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / &
                                      ( iv%ssmt1(n)%t(k)%error * &
                                        iv%ssmt1(n)%t(k)%error )

            jo % ssmt1_t = jo % ssmt1_t - &
                           re%ssmt1(n)%t(k) * jo_grad_y%ssmt1(n)%t(k)
         end do
      END DO

      jo % ssmt1_t = 0.5 * jo % ssmt1_t

   END IF

END SUBROUTINE da_cal_jo_and_grady_ssmt1

