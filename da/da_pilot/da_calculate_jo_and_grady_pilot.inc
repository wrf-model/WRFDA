subroutine da_calculate_jo_and_grady_pilot(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.


   integer                      :: n, k

   jo % pilot_u = 0.0
   jo % pilot_v = 0.0

   if (iv%num_pilot > 0) then
      do n=1, iv%num_pilot
         do k=1, iv%pilot(n)%info%levels
            jo_grad_y%pilot(n)%u(k) = -re%pilot(n)%u(k) / &
                                    (iv%pilot(n)%u(k)%error * &
                                      iv%pilot(n)%u(k)%error)
            jo_grad_y%pilot(n)%v(k) = -re%pilot(n)%v(k) / &
                                    (iv%pilot(n)%v(k)%error * &
                                      iv%pilot(n)%v(k)%error)
         end do

        if (iv%pilot(n)%loc%proc_domain) then
           do k=1, iv%pilot(n)%info%levels
              jo % pilot_u = jo % pilot_u - &
                          re%pilot(n)%u(k) * jo_grad_y%pilot(n)%u(k)
              jo % pilot_v = jo % pilot_v - &
                          re%pilot(n)%v(k) * jo_grad_y%pilot(n)%v(k)
           end do
         end if
      end do

      jo % pilot_u = 0.5 * jo % pilot_u
      jo % pilot_v = 0.5 * jo % pilot_v

   end if

end subroutine da_calculate_jo_and_grady_pilot


