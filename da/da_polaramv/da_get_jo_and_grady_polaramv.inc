subroutine da_get_jo_and_grady_polaramv( iv, re, jo, jo_grad_y )

   !----------------------------------------------------------------------
   ! Purpose:  Calculates Cost function and Gradient for Polar AMVs Obs
   !----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n, k

   jo % polaramv_u = 0.0
   jo % polaramv_v = 0.0

   if ( iv%num_polaramv > 0 ) then
      do n=1, iv%num_polaramv
         do k=1, iv%polaramv(n)%info%levels

            jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
                                      ( iv%polaramv(n)%u(k)%error * &
                                        iv%polaramv(n)%u(k)%error )
            jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
                                      ( iv%polaramv(n)%v(k)%error * &
                                        iv%polaramv(n)%v(k)%error )
         end do

         if (iv%polaramv(n)%loc%proc_domain) then
            do k=1, iv%polaramv(n)%info%levels
               jo % polaramv_u = jo % polaramv_u - &
                        re%polaramv(n)%u(k) * jo_grad_y%polaramv(n)%u(k)
               jo % polaramv_v = jo % polaramv_v - &
                         re%polaramv(n)%v(k) * jo_grad_y%polaramv(n)%v(k)
            end do
         end if
      end do

      jo % polaramv_u = 0.5 * jo % polaramv_u
      jo % polaramv_v = 0.5 * jo % polaramv_v

   end if
     
end subroutine da_get_jo_and_grady_polaramv

