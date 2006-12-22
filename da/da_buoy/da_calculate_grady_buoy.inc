subroutine da_calculate_grady_buoy( iv, re, jo_grad_y )

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                       :: n

   if ( iv%num_buoy > 0 ) then
      do n=1, iv%num_buoy
         if(iv%buoy(n)%u%qc < obs_qc_pointer) re%buoy(n)%u = 0.0
         if(iv%buoy(n)%v%qc < obs_qc_pointer) re%buoy(n)%v = 0.0
         if(iv%buoy(n)%t%qc < obs_qc_pointer) re%buoy(n)%t = 0.0
         if(iv%buoy(n)%p%qc < obs_qc_pointer) re%buoy(n)%p = 0.0
         if(iv%buoy(n)%q%qc < obs_qc_pointer) re%buoy(n)%q = 0.0

         jo_grad_y%buoy(n)%u = -re%buoy(n)%u / &
                                ( iv%buoy(n)%u%error * iv%buoy(n)%u%error)
         jo_grad_y%buoy(n)%v = -re%buoy(n)%v / &
                                ( iv%buoy(n)%v%error * iv%buoy(n)%v%error)
         jo_grad_y%buoy(n)%t = -re%buoy(n)%t / &
                                ( iv%buoy(n)%t%error * iv%buoy(n)%t%error)
         jo_grad_y%buoy(n)%p = -re%buoy(n)%p / &
                                ( iv%buoy(n)%p%error * iv%buoy(n)%p%error)
         jo_grad_y%buoy(n)%q = -re%buoy(n)%q / &
                                ( iv%buoy(n)%q%error * iv%buoy(n)%q%error)
      end do
   end if
     
end subroutine da_calculate_grady_buoy


