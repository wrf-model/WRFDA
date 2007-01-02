SUBROUTINE DA_Calculate_GradY_Profiler( iv, re, jo_grad_y )
!------------------------------------------------------------------------------
!  HISTORY: 
!
!      Created: 02/17/2003                   S. R. H. Rizvi
!
!      Purpose: Applies obs inverse on re-vector
!               
!------------------------------------------------------------------------------
   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)

   INTEGER                      :: n, k
   IF ( iv%num_profiler > 0 ) THEN
   
      DO n=1, iv%num_profiler

       if( iv%profiler(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%profiler(n)%info%levels

        if(iv%profiler(n)%u(k)%qc < obs_qc_pointer) re%profiler(n)%u(k) = 0.0
        if(iv%profiler(n)%v(k)%qc < obs_qc_pointer) re%profiler(n)%v(k) = 0.0

            jo_grad_y%profiler(n)%u(k) = -re%profiler(n)%u(k) / &
                                      ( iv%profiler(n)%u(k)%error * &
                                        iv%profiler(n)%u(k)%error )
            jo_grad_y%profiler(n)%v(k) = -re%profiler(n)%v(k) / &
                                      ( iv%profiler(n)%v(k)%error * &
                                        iv%profiler(n)%v(k)%error )
         end do
       
       endif

      END DO
      
   END IF

END SUBROUTINE DA_Calculate_GradY_Profiler

