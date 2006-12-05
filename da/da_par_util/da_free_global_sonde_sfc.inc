subroutine da_free_global_sonde_sfc( iv_glob, re_glob, jo_grad_y_glob )

   implicit none

   ! task-global objects
   type (ob_type), intent(inout) :: iv_glob        ! Innovation vector
   type (y_type),  intent(inout) :: re_glob        ! residual vector
   type (y_type),  intent(inout) :: jo_grad_y_glob ! Grad_y(Jo)
   ! Local declarations
   deallocate( iv_glob%sonde_sfc )
   deallocate( re_glob%sonde_sfc )
   deallocate( jo_grad_y_glob%sonde_sfc )
   
end subroutine da_free_global_sonde_sfc
