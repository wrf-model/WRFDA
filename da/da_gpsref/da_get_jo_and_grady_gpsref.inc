subroutine da_get_jo_and_grady_gpsref( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   !----------------------------------------------------------------------
   ! Called by da_minimisation/da_calculate_jo_and_grady.inc
   !----------------------------------------------------------------------

   implicit none

   type(ob_type), intent(in)   :: iv          ! Innovation vector.
   type(y_type), intent(in)    :: re          ! Residual vector.
   type(y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n, k

   jo % gpsref_ref = 0.0

   if (iv%num_GPSRef > 0) then
      do n=1, iv%num_GPSRef
         do k=1, iv%gpsref(n)%info%levels
            jo_grad_y%GPSRef(n)%ref(k) = -re%GPSRef(n)%ref(k) / &
                                  ( iv%GPSRef(n)%ref(k)%error * &
                                     iv%GPSRef(n)%ref(k)%error)
         end do

         if (iv%gpsref(n)%loc%proc_domain) then
            do k=1, iv%gpsref(n)%info%levels
               jo % gpsref_ref = jo % gpsref_ref - &
                   re%GPSRef(n)%ref(k) * jo_grad_y%GPSRef(n)%ref(k)
            end do
         end if
      end do
      
      jo % gpsref_ref = 0.5 * jo % gpsref_ref
      
   end if

end subroutine da_get_jo_and_grady_gpsref


