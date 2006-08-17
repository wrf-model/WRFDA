subroutine da_free_global_sonde_sfc( iv_glob, re_glob, jo_grad_y_glob )

   IMPLICIT NONE

   ! task-global objects
   TYPE (ob_type), INTENT(INOUT) :: iv_glob        ! Innovation vector
   TYPE (y_type),  INTENT(INOUT) :: re_glob        ! residual vector
   TYPE (y_type),  INTENT(INOUT) :: jo_grad_y_glob ! Grad_y(Jo)
   ! Local declarations
   DEALLOCATE( iv_glob%sonde_sfc )
   DEALLOCATE( re_glob%sonde_sfc )
   DEALLOCATE( jo_grad_y_glob%sonde_sfc )
   
end subroutine da_free_global_sonde_sfc
