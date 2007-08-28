subroutine da_jo_and_grady_geoamv( iv, re, jo, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose:  Calculates Cost function and Gradient for Geo. CVMs 
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv          ! Innovation vector.
   type(y_type),  intent(in)    :: re          ! Residual vector.
   type(y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_geoamv")

   jo % geoamv_u = 0.0
   jo % geoamv_v = 0.0

   if (iv%nlocal(geoamv) > 0) then
      do n=1, iv%nlocal(geoamv)
         do k=1, iv%geoamv(n)%info%levels
            jo_grad_y%geoamv(n)%u(k) = -re%geoamv(n)%u(k) / &
                                        ( iv%geoamv(n)%u(k)%error * &
                                           iv%geoamv(n)%u(k)%error)
            jo_grad_y%geoamv(n)%v(k) = -re%geoamv(n)%v(k) / &
                                        ( iv%geoamv(n)%v(k)%error * &
                                           iv%geoamv(n)%v(k)%error)
         end do

         if (iv%geoamv(n)%loc%proc_domain) then
            do k=1, iv%geoamv(n)%info%levels
               jo % geoamv_u = jo % geoamv_u - &
                            re%geoamv(n)%u(k) * jo_grad_y%geoamv(n)%u(k)
               jo % geoamv_v = jo % geoamv_v - &
                            re%geoamv(n)%v(k) * jo_grad_y%geoamv(n)%v(k)
            end do
         end if
      end do

      jo % geoamv_u = 0.5 * jo % geoamv_u
      jo % geoamv_v = 0.5 * jo % geoamv_v
   end if

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_geoamv")
     
end subroutine da_jo_and_grady_geoamv


