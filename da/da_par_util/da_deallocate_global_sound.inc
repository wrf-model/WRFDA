subroutine da_deallocate_global_sound(iv_glob, re_glob, jo_grad_y_glob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   ! task-global objects
   type (iv_type), intent(inout) :: iv_glob        ! Innovation vector
   type (y_type),  intent(inout) :: re_glob        ! residual vector
   type (y_type),  intent(inout) :: jo_grad_y_glob ! Grad_y(Jo)
   ! Local declarations
   integer :: n
   deallocate(iv_glob%sound)
   do n=1,size(re_glob%sound)
      deallocate(re_glob%sound(n)%u, re_glob%sound(n)%v, &
                  re_glob%sound(n)%t, re_glob%sound(n)%q)
   end do
   deallocate(re_glob%sound)
   do n=1,size(jo_grad_y_glob%sound)
      deallocate(jo_grad_y_glob%sound(n)%u, jo_grad_y_glob%sound(n)%v, &
                  jo_grad_y_glob%sound(n)%t, jo_grad_y_glob%sound(n)%q)
   end do
   deallocate(jo_grad_y_glob%sound)

end subroutine da_deallocate_global_sound


