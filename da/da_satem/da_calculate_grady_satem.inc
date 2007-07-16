subroutine da_calculate_grady_satem(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none


   type (ob_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)


   integer                      :: n, k

   if (iv%num_satem > 0) then
      do n=1, iv%num_satem
            do k=1, iv%satem(n)%info%levels
               if (iv%satem(n)%thickness(k)%qc < obs_qc_pointer) then
                  re%satem(n)%thickness(k) = 0.0
               end if

               jo_grad_y%satem(n)%thickness(k) = -re%satem(n)%thickness(k) / &
                                         (iv%satem(n)%thickness(k)%error * &
                                           iv%satem(n)%thickness(k)%error)
            end do
      end do
   end if

end subroutine da_calculate_grady_satem


