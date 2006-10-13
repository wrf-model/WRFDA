subroutine da_get_jo_and_grady_geoamv( iv, re, jo, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose:  Calculates Cost function and Gradient for Geo. CVMs 
   !-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % geoamv_u = 0.0
   jo % geoamv_v = 0.0

   IF (iv%num_geoamv > 0) THEN
      DO n=1, iv%num_geoamv
         do k=1, iv%geoamv(n)%info%levels
            jo_grad_y%geoamv(n)%u(k) = -re%geoamv(n)%u(k) / &
                                         ( iv%geoamv(n)%u(k)%error * &
                                           iv%geoamv(n)%u(k)%error )
            jo_grad_y%geoamv(n)%v(k) = -re%geoamv(n)%v(k) / &
                                         ( iv%geoamv(n)%v(k)%error * &
                                           iv%geoamv(n)%v(k)%error )
         end do

         if (iv%geoamv(n)%loc%proc_domain) then
            do k=1, iv%geoamv(n)%info%levels
               jo % geoamv_u = jo % geoamv_u - &
                            re%geoamv(n)%u(k) * jo_grad_y%geoamv(n)%u(k)
               jo % geoamv_v = jo % geoamv_v - &
                            re%geoamv(n)%v(k) * jo_grad_y%geoamv(n)%v(k)
            end do
         end if
      END DO

      jo % geoamv_u = 0.5 * jo % geoamv_u
      jo % geoamv_v = 0.5 * jo % geoamv_v

   END IF
     
end subroutine da_get_jo_and_grady_geoamv

