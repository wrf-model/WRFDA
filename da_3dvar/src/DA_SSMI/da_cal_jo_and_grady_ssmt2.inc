subroutine da_cal_jo_and_grady_ssmt2( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % ssmt2_rh = 0.0

   IF ( iv%num_ssmt2 > 0 ) THEN
   
      DO n=1, iv%num_ssmt2
         do k=1, iv%ssmt2(n)%info%levels

            jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
                                      ( iv%ssmt2(n)%rh(k)%error * &
                                        iv%ssmt2(n)%rh(k)%error )

            jo % ssmt2_rh = jo % ssmt2_rh - &
                            re%ssmt2(n)%rh(k) * jo_grad_y%ssmt2(n)%rh(k)
         end do
      END DO

      jo % ssmt2_rh = 0.5 * jo % ssmt2_rh

   END IF

END subroutine da_cal_jo_and_grady_ssmt2

