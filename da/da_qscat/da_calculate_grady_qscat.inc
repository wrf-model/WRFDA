subroutine da_calculate_grady_qscat( iv, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n

   IF (iv%num_qscat > 0) THEN
      DO n=1, iv%num_qscat
         if(iv%qscat(n)%u%qc < obs_qc_pointer) re%qscat(n)%u = 0.0
         if(iv%qscat(n)%v%qc < obs_qc_pointer) re%qscat(n)%v = 0.0
         jo_grad_y%qscat(n)%u = -re%qscat(n)%u / &
                               ( iv%qscat(n)%u%error * iv%qscat(n)%u%error)
         jo_grad_y%qscat(n)%v = -re%qscat(n)%v / &
                               ( iv%qscat(n)%v%error * iv%qscat(n)%v%error)
      END DO
   END IF
     
end subroutine da_calculate_grady_qscat



