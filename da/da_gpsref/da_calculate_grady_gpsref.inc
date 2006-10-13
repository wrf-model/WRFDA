subroutine da_calculate_grady_gpsref( iv, re, jo_grad_y )

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector         
   !----------------------------------------------------------------------

   IMPLICIT NONE


   TYPE (ob_type), INTENT(IN)     :: iv          ! Innovation vector.
   TYPE (y_type),  INTENT(INOUT)  :: re          ! Residual vector.
   TYPE (y_type),  INTENT(INOUT)  :: jo_grad_y   ! Grad_y(Jo)


   INTEGER :: n, k

   IF ( iv%num_GPSRef < 1 ) return
   
   DO n=1, iv%num_GPSRef
      do k=1, iv%gpsref(n)%info%levels
         if (iv%GPSRef(n)%ref(k)%qc < obs_qc_pointer) then
            re%GPSRef(n)%ref(k) = 0.0
         end if
         jo_grad_y%GPSRef(n)%ref(k) = -re%GPSRef(n)%ref(k) / &
                                   ( iv%GPSRef(n)%ref(k)%error * &
                                     iv%GPSRef(n)%ref(k)%error )
      end do
   END DO

end subroutine da_calculate_grady_gpsref

