subroutine da_calculate_grady_rad( iv, re, jo_grad_y ) 
   !------------------------------------------------------------------------------
   !  PURPOSE: Calculate Gradient_y for radiance data.
   !
   !  METHOD:  grad_y = -R^-1 (d - H delta_x ).
   !------------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! innovation vector.
   type (y_type) , intent(in)    :: re          ! residual vector.
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)

   integer                       :: n, k, i

   if (iv%num_inst < 1) return

   if (trace_use) call da_trace_entry("da_calculate_grady_rad")

   do i =1, iv%num_inst
      if ( iv%instid(i)%num_rad < 1 ) cycle
      do n=1, iv%instid(i)%num_rad
         do k=1, iv%instid(i)%nchan
            jo_grad_y%instid(i)%rad(n)%tb(k) = -re%instid(i)%rad(n)%tb(k) / &
                                      ( iv%instid(i)%rad(n)%tb(k)%error * &
                                        iv%instid(i)%rad(n)%tb(k)%error )
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_calculate_grady_rad")

end subroutine da_calculate_grady_rad


