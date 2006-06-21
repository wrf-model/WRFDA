subroutine da_free_global_synop( iv_glob, re_glob, jo_grad_y_glob )

    IMPLICIT NONE

    ! task-global objects
    TYPE (ob_type), INTENT(INOUT) :: iv_glob        ! Innovation vector
    TYPE (y_type),  INTENT(INOUT) :: re_glob        ! residual vector
    TYPE (y_type),  INTENT(INOUT) :: jo_grad_y_glob ! Grad_y(Jo)
    ! Local declarations
    DEALLOCATE( iv_glob%synop )
    DEALLOCATE( re_glob%synop )
    DEALLOCATE( jo_grad_y_glob%synop )
    RETURN
end subroutine da_free_global_synop

