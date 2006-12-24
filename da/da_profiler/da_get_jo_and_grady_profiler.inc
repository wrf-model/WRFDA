subroutine da_jo_and_grady_profiler( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(ob_type), intent(in)   :: iv          ! Innovation vector.
   type(y_type), intent(in)    :: re          ! Residual vector.
   type(y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.


   integer                      :: n, k

   jo % profiler_u = 0.0
   jo % profiler_v = 0.0

   if (iv%num_profiler > 0) then
      do n=1, iv%num_profiler
         do k=1, iv%profiler(n)%info%levels
            jo_grad_y%profiler(n)%u(k) = -re%profiler(n)%u(k) / &
                                     ( iv%profiler(n)%u(k)%error * &
                                        iv%profiler(n)%u(k)%error)
            jo_grad_y%profiler(n)%v(k) = -re%profiler(n)%v(k) / &
                                     ( iv%profiler(n)%v(k)%error * &
                                        iv%profiler(n)%v(k)%error)
         end do

         if (iv%profiler(n)%loc%proc_domain) then
            do k=1, iv%profiler(n)%info%levels
               jo % profiler_u = jo % profiler_u - &
                         re%profiler(n)%u(k) * jo_grad_y%profiler(n)%u(k)
               jo % profiler_v = jo % profiler_v - &
                         re%profiler(n)%v(k) * jo_grad_y%profiler(n)%v(k)
            end do
         end if
      end do
      
      jo % profiler_u = 0.5 * jo % profiler_u
      jo % profiler_v = 0.5 * jo % profiler_v
      
   end if

end subroutine da_jo_and_grady_profiler


