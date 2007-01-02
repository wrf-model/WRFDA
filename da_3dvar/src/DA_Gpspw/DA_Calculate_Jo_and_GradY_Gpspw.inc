SUBROUTINE DA_Calculate_Jo_and_GradY_Gpspw( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n

   jo % gpspw_tpw = 0.0

   IF ( iv%num_Gpspw > 0 ) THEN
   
      DO n=1, iv%num_Gpspw

       if( iv%gpspw(n)%loc%proc_domain_with_halo ) then

         jo_grad_y%Gpspw(n)%tpw = -re%Gpspw(n)%tpw / &
                                  ( iv%Gpspw(n)%tpw%error * &
                                    iv%Gpspw(n)%tpw%error)
       endif

       if( iv%gpspw(n)%loc%proc_domain ) then

         jo % gpspw_tpw = jo % gpspw_tpw - re%Gpspw(n)%tpw * jo_grad_y%Gpspw(n)%tpw

       endif

      END DO

      jo % gpspw_tpw = 0.5 * jo % gpspw_tpw

   END IF

END SUBROUTINE DA_Calculate_Jo_and_GradY_Gpspw

