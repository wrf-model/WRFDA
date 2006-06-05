SUBROUTINE DA_Calculate_Jo_and_GradY_Radar( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   TYPE (jo_type),INTENT(INOUT) :: jo          ! Obs cost function.


   INTEGER                      :: n, k

   jo % Radar_rv = 0.0
   jo % Radar_rf = 0.0

   IF ( iv%num_Radar < 0 ) RETURN
   
      DO n=1, iv%num_Radar

       if( iv%Radar(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%Radar(n)%info%levels

            IF ( USE_Radar_rv ) THEN
            jo_grad_y%Radar(n)%rv(k) = -re%Radar(n)%rv(k) / &
                                      ( iv%Radar(n)%rv(k)%error * &
                                        iv%Radar(n)%rv(k)%error )
            ENDIF

            IF ( USE_Radar_rf ) THEN
            jo_grad_y%Radar(n)%rf(k) = -re%Radar(n)%rf(k) / &
                                      ( iv%Radar(n)%rf(k)%error * &
                                        iv%Radar(n)%rf(k)%error )
            ENDIF

         end do

       end if

       if( iv%Radar(n)%loc%proc_domain ) then

         do k=1, iv%Radar(n)%info%levels

            IF ( USE_Radar_rv ) THEN
            jo % Radar_rv = jo % Radar_rv - &
                         re%Radar(n)%rv(k) * jo_grad_y%Radar(n)%rv(k)
            ENDIF

            IF ( USE_Radar_rf ) THEN
            jo % Radar_rf = jo % Radar_rf - &
                         re%Radar(n)%rf(k) * jo_grad_y%Radar(n)%rf(k)
            ENDIF

         end do

       endif

      END DO
      
   jo % Radar_rv = 0.5 * jo % Radar_rv
   jo % Radar_rf = 0.5 * jo % Radar_rf


END SUBROUTINE DA_Calculate_Jo_and_GradY_Radar

