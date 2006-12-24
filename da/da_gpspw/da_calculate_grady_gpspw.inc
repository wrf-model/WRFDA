subroutine da_calculate_grady_gpspw(iv, re, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(inout) :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n


   if (iv%num_Gpspw > 0) then
      do n=1, iv%num_Gpspw
         if (iv%gpspw(n)%tpw%qc < obs_qc_pointer) then
            re%gpspw(n)%tpw = 0.0
         end if

         jo_grad_y%Gpspw(n)%tpw = -re%Gpspw(n)%tpw / &
                                    (iv%Gpspw(n)%tpw%error * &
                                      iv%Gpspw(n)%tpw%error)
      end do
   end if

end subroutine da_calculate_grady_gpspw


