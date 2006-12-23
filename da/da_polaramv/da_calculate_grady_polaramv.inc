subroutine da_calculate_grady_polaramv( iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates Gradient of Polar AMVs  Obs.          
   !-------------------------------------------------------------------------

   implicit none


   type (ob_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n , k

   if (iv%num_polaramv > 0) then
      do n=1, iv%num_polaramv
         do k=1, iv%polaramv(n)%info%levels
            if (iv%polaramv(n)%u(k)%qc < obs_qc_pointer) then
               re%polaramv(n)%u(k) = 0.0
            end if
            if (iv%polaramv(n)%v(k)%qc < obs_qc_pointer) then
               re%polaramv(n)%v(k) = 0.0
            end if

            jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
                                    ( iv%polaramv(n)%u(k)%error * &
                                      iv%polaramv(n)%u(k)%error)
            jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
                                    ( iv%polaramv(n)%v(k)%error * &
                                      iv%polaramv(n)%v(k)%error)
         end do
      end do
   end if

end subroutine da_calculate_grady_polaramv


