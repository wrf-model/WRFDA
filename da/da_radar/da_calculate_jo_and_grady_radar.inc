subroutine da_calculate_jo_and_grady_radar( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type),intent(inout) :: jo          ! Obs cost function.

   integer                      :: n, k

   jo % Radar_rv = 0.0
   jo % Radar_rf = 0.0

   if ( iv%num_Radar < 0 ) return
   
   do n=1, iv%num_Radar
      do k=1, iv%Radar(n)%info%levels
         if ( use_Radar_rv ) then
            jo_grad_y%Radar(n)%rv(k) = -re%Radar(n)%rv(k) / &
                                    ( iv%Radar(n)%rv(k)%error * &
                                      iv%Radar(n)%rv(k)%error )
         end if

         if ( use_Radar_rf ) then
            jo_grad_y%Radar(n)%rf(k) = -re%Radar(n)%rf(k) / &
                                    ( iv%Radar(n)%rf(k)%error * &
                                      iv%Radar(n)%rf(k)%error )
         end if
      end do

      if (iv%Radar(n)%loc%proc_domain) then
         do k=1, iv%Radar(n)%info%levels
            if ( use_Radar_rv ) then
               jo % Radar_rv = jo % Radar_rv - &
                         re%Radar(n)%rv(k) * jo_grad_y%Radar(n)%rv(k)
            end if

            if ( use_Radar_rf ) then
               jo % Radar_rf = jo % Radar_rf - &
                         re%Radar(n)%rf(k) * jo_grad_y%Radar(n)%rf(k)
            end if
         end do
      end if
   end do
      
   jo % Radar_rv = 0.5 * jo % Radar_rv
   jo % Radar_rf = 0.5 * jo % Radar_rf

end subroutine da_calculate_jo_and_grady_radar

