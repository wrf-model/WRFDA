subroutine da_get_jo_and_grady_gpsref( iv, re, jo, jo_grad_y )

   !----------------------------------------------------------------------
   ! Called by da_minimisation/da_calculate_jo_and_grady.inc
   !----------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % gpsref_ref = 0.0

   IF (iv%num_GPSRef > 0) THEN
      DO n=1, iv%num_GPSRef
         do k=1, iv%gpsref(n)%info%levels
            jo_grad_y%GPSRef(n)%ref(k) = -re%GPSRef(n)%ref(k) / &
                                   ( iv%GPSRef(n)%ref(k)%error * &
                                     iv%GPSRef(n)%ref(k)%error )
         end do

         if (iv%gpsref(n)%loc%proc_domain) then
            do k=1, iv%gpsref(n)%info%levels
               jo % gpsref_ref = jo % gpsref_ref - &
                   re%GPSRef(n)%ref(k) * jo_grad_y%GPSRef(n)%ref(k)
            end do
         endif
      END DO
      
      jo % gpsref_ref = 0.5 * jo % gpsref_ref
      
   END IF

end subroutine da_get_jo_and_grady_gpsref

