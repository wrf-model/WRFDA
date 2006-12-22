subroutine da_calculate_jo_and_grady_qscat( iv, re, jo, jo_grad_y )

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n

   jo % qscat_u = 0.0
   jo % qscat_v = 0.0

   if ( iv%num_qscat > 0 ) then

      do n=1, iv%num_qscat
         jo_grad_y%qscat(n)%u = -re%qscat(n)%u / &
                                  ( iv%qscat(n)%u%error * iv%qscat(n)%u%error)
          jo_grad_y%qscat(n)%v = -re%qscat(n)%v / &
                                  ( iv%qscat(n)%v%error * iv%qscat(n)%v%error)
        
         if ( iv%qscat(n)%loc%proc_domain ) then
            jo % qscat_u = jo % qscat_u - re%qscat(n)%u * jo_grad_y%qscat(n)%u
            jo % qscat_v = jo % qscat_v - re%qscat(n)%v * jo_grad_y%qscat(n)%v
         end if
      end do
      
      jo % qscat_u = 0.5 * jo % qscat_u
      jo % qscat_v = 0.5 * jo % qscat_v

   end if
     
end subroutine da_calculate_jo_and_grady_qscat


