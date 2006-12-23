subroutine da_calculate_grady_radar( iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k

   if (iv%num_Radar > 0) then 
      do n=1, iv%num_Radar
         do k=1, iv%Radar(n)%info%levels
            if (use_Radar_rv) then
               if (iv%Radar(n)%rv(k)%qc < obs_qc_pointer) then
                  re%Radar(n)%rv(k) = 0.0
               end if

               jo_grad_y%Radar(n)%rv(k) = -re%Radar(n)%rv(k) / &
                                      ( iv%Radar(n)%rv(k)%error * &
                                        iv%Radar(n)%rv(k)%error)
            end if

            if (use_Radar_rf) then
               if (iv%Radar(n)%rf(k)%qc < obs_qc_pointer) then
                  re%Radar(n)%rf(k) = 0.0
               end if
               jo_grad_y%Radar(n)%rf(k) = -re%Radar(n)%rf(k) / &
                                      ( iv%Radar(n)%rf(k)%error * &
                                        iv%Radar(n)%rf(k)%error)
            end if
         end do
      end do
   end if

end subroutine da_calculate_grady_radar


