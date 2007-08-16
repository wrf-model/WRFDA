subroutine da_jo_sound_uvtq(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both 
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in  ) :: iv         ! Innovation vector.
   type (y_type),  intent(in  ) :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.
 
   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_sound_uvtq")

   do n=1, iv%nlocal(sound)
      if (iv%sound(n)%loc%proc_domain) then
         do k=1, iv%sound(n)%info%levels
            jo % sound_u = jo % sound_u - re%sound(n)%u(k) * jo_grad_y%sound(n)%u(k)
            jo % sound_v = jo % sound_v - re%sound(n)%v(k) * jo_grad_y%sound(n)%v(k)
            jo % sound_t = jo % sound_t - re%sound(n)%t(k) * jo_grad_y%sound(n)%t(k)
            jo % sound_q = jo % sound_q - re%sound(n)%q(k) * jo_grad_y%sound(n)%q(k)
         end do
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_sound_uvtq")

end subroutine da_jo_sound_uvtq


