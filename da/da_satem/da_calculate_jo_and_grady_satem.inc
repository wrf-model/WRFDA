subroutine da_calculate_jo_and_grady_satem( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n, k

   jo % satem_thickness = 0.0

   if ( iv%num_Satem > 0 ) then
      do n=1, iv%num_Satem
         do k=1, iv%satem(n)%info%levels
            jo_grad_y%Satem(n)%thickness(k) = -re%Satem(n)%thickness(k) / &
                                        ( iv%Satem(n)%thickness(k)%error * &
                                          iv%Satem(n)%thickness(k)%error )
         end do

         if (iv%satem(n)%loc%proc_domain ) then
            do k=1, iv%satem(n)%info%levels
               jo % satem_thickness = jo % satem_thickness - &
                     re%Satem(n)%thickness(k) * jo_grad_y%Satem(n)%thickness(k)
            end do
         end if
      end do

      jo % satem_thickness = 0.5 * jo % satem_thickness

   end if

end subroutine da_calculate_jo_and_grady_satem

