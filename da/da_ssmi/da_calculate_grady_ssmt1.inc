subroutine da_calculate_grady_ssmt1( iv, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(inout) :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k


   if ( iv%num_ssmt1 > 0 ) then  
      do n=1, iv%num_ssmt1
         do k=1, iv%ssmt1(n)%info%levels

            if(iv%ssmt1(n)%t(k)%qc < obs_qc_pointer) re%ssmt1(n)%t(k) = 0.0
            jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / &
                                      ( iv%ssmt1(n)%t(k)%error * &
                                        iv%ssmt1(n)%t(k)%error )

         end do
      end do
   end if

end subroutine da_calculate_grady_ssmt1


