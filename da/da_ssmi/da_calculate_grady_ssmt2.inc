subroutine da_calculate_grady_ssmt2( iv, re, jo_grad_y )

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(inout) :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k

   if ( iv%num_ssmt2 > 0 ) then  
      do n=1, iv%num_ssmt2
         do k=1, iv%ssmt2(n)%info%levels
            if(iv%ssmt2(n)%rh(k)%qc < obs_qc_pointer) re%ssmt2(n)%rh(k) = 0.0

            jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
                                      ( iv%ssmt2(n)%rh(k)%error * &
                                        iv%ssmt2(n)%rh(k)%error )

         end do
      end do
   end if

end subroutine da_calculate_grady_ssmt2

