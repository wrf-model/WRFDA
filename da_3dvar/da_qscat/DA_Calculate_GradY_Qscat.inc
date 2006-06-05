subroutine DA_Calculate_GradY_Qscat( iv, re, jo_grad_y )
!-------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!               
!-------------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n

   IF ( iv%num_qscat > 0 ) THEN

      DO n=1, iv%num_qscat

        if ( iv%qscat(n)%loc%proc_domain_with_halo ) then

       if(iv%qscat(n)%u%qc < obs_qc_pointer) re%qscat(n)%u = 0.0
       if(iv%qscat(n)%v%qc < obs_qc_pointer) re%qscat(n)%v = 0.0


           jo_grad_y%qscat(n)%u = -re%qscat(n)%u / &
                                  ( iv%qscat(n)%u%error * iv%qscat(n)%u%error)
           jo_grad_y%qscat(n)%v = -re%qscat(n)%v / &
                                  ( iv%qscat(n)%v%error * iv%qscat(n)%v%error)
        endif

      END DO

   END IF
     
end subroutine DA_Calculate_GradY_Qscat



