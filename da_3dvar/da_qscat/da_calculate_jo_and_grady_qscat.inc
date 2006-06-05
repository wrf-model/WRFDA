subroutine da_calculate_jo_and_grady_qscat( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % qscat_u = 0.0
   jo % qscat_v = 0.0

   IF ( iv%num_qscat > 0 ) THEN

      DO n=1, iv%num_qscat

        if ( iv%qscat(n)%loc%proc_domain_with_halo ) then

           jo_grad_y%qscat(n)%u = -re%qscat(n)%u / &
                                  ( iv%qscat(n)%u%error * iv%qscat(n)%u%error)
           jo_grad_y%qscat(n)%v = -re%qscat(n)%v / &
                                  ( iv%qscat(n)%v%error * iv%qscat(n)%v%error)
        endif

        if ( iv%qscat(n)%loc%proc_domain ) then
           jo % qscat_u = jo % qscat_u - re%qscat(n)%u * jo_grad_y%qscat(n)%u
           jo % qscat_v = jo % qscat_v - re%qscat(n)%v * jo_grad_y%qscat(n)%v
        endif

      END DO
      
      jo % qscat_u = 0.5 * jo % qscat_u
      jo % qscat_v = 0.5 * jo % qscat_v

   END IF
     
end subroutine da_calculate_jo_and_grady_qscat

