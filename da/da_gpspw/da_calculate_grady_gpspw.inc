subroutine da_calculate_grady_gpspw( iv, re, jo_grad_y )

   !-----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-----------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(INOUT) :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n


   IF ( iv%num_Gpspw > 0 ) THEN
      DO n=1, iv%num_Gpspw
         if (iv%gpspw(n)%tpw%qc < obs_qc_pointer) THEN
            re%gpspw(n)%tpw = 0.0
         END IF

         jo_grad_y%Gpspw(n)%tpw = -re%Gpspw(n)%tpw / &
                                    ( iv%Gpspw(n)%tpw%error * &
                                      iv%Gpspw(n)%tpw%error)
      END DO
   END IF

end subroutine da_calculate_grady_gpspw

