subroutine da_calculate_grady_metar( iv, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(INOUT) :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n

   IF (trace_use) call da_trace_entry("da_calculate_grady_metar")

   IF ( iv%num_metar > 0 ) THEN
      DO n=1, iv%num_metar
         if(iv%metar(n)%u%qc < obs_qc_pointer) re%metar(n)%u = 0.0
         if(iv%metar(n)%v%qc < obs_qc_pointer) re%metar(n)%v = 0.0
         if(iv%metar(n)%t%qc < obs_qc_pointer) re%metar(n)%t = 0.0
         if(iv%metar(n)%p%qc < obs_qc_pointer) re%metar(n)%p = 0.0
         if(iv%metar(n)%q%qc < obs_qc_pointer) re%metar(n)%q = 0.0

         jo_grad_y%metar(n)%u = -re%metar(n)%u / &
                               ( iv%metar(n)%u%error * iv%metar(n)%u%error)
         jo_grad_y%metar(n)%v = -re%metar(n)%v / &
                               ( iv%metar(n)%v%error * iv%metar(n)%v%error)
         jo_grad_y%metar(n)%t = -re%metar(n)%t / &
                               ( iv%metar(n)%t%error * iv%metar(n)%t%error)
         jo_grad_y%metar(n)%p = -re%metar(n)%p / &
                               ( iv%metar(n)%p%error * iv%metar(n)%p%error)
         jo_grad_y%metar(n)%q = -re%metar(n)%q / &
                               ( iv%metar(n)%q%error * iv%metar(n)%q%error)
      END DO
   END IF

   IF (trace_use) call da_trace_exit("da_calculate_grady_metar")
     
end subroutine da_calculate_grady_metar



