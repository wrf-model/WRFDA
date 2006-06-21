subroutine da_free_global_sound( iv_glob, re_glob, jo_grad_y_glob )

    IMPLICIT NONE

    ! task-global objects
    TYPE (ob_type), INTENT(INOUT) :: iv_glob        ! Innovation vector
    TYPE (y_type),  INTENT(INOUT) :: re_glob        ! residual vector
    TYPE (y_type),  INTENT(INOUT) :: jo_grad_y_glob ! Grad_y(Jo)
    ! Local declarations
    INTEGER :: n
    DEALLOCATE( iv_glob%sound )
    DO n=1,SIZE(re_glob%sound)
      DEALLOCATE( re_glob%sound(n)%u, re_glob%sound(n)%v, &
                  re_glob%sound(n)%t, re_glob%sound(n)%q )
    ENDDO
    DEALLOCATE( re_glob%sound )
    DO n=1,SIZE(jo_grad_y_glob%sound)
      DEALLOCATE( jo_grad_y_glob%sound(n)%u, jo_grad_y_glob%sound(n)%v, &
                  jo_grad_y_glob%sound(n)%t, jo_grad_y_glob%sound(n)%q )
    ENDDO
    DEALLOCATE( jo_grad_y_glob%sound )
    RETURN
end subroutine da_free_global_sound
