subroutine da_calculate_grady_gpsref( iv, re, jo_grad_y )

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector         
   !----------------------------------------------------------------------

   implicit none


   type (ob_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)


   integer :: n, k

   if ( iv%num_GPSRef < 1 ) return
   
   do n=1, iv%num_GPSRef
      do k=1, iv%gpsref(n)%info%levels
         if (iv%GPSRef(n)%ref(k)%qc < obs_qc_pointer) then
            re%GPSRef(n)%ref(k) = 0.0
         end if
         jo_grad_y%GPSRef(n)%ref(k) = -re%GPSRef(n)%ref(k) / &
                                   ( iv%GPSRef(n)%ref(k)%error * &
                                     iv%GPSRef(n)%ref(k)%error )
      end do
   end do

end subroutine da_calculate_grady_gpsref

