subroutine da_calculate_jo_and_grady_gpspw(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n

   jo % gpspw_tpw = 0.0

   if (iv%num_Gpspw > 0) then
      do n=1, iv%num_Gpspw
         jo_grad_y%Gpspw(n)%tpw = -re%Gpspw(n)%tpw / &
                                   (iv%Gpspw(n)%tpw%error * &
                                     iv%Gpspw(n)%tpw%error)

         if (iv%gpspw(n)%loc%proc_domain) then
           jo % gpspw_tpw = jo % gpspw_tpw - re%Gpspw(n)%tpw * &
              jo_grad_y%Gpspw(n)%tpw
         end if
      end do

      jo % gpspw_tpw = 0.5 * jo % gpspw_tpw
   end if

end subroutine da_calculate_jo_and_grady_gpspw


