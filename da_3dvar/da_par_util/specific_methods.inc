

!------------------------------------------------------------------------------
! PURPOSE:  Free objects created by to_global_sound().  
!
! METHOD:   Simple bookeeping.  
!
!  HISTORY:
!           09/22/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: par_util
!------------------------------------------------------------------------------
  SUBROUTINE free_global_sound( iv_glob, re_glob, jo_grad_y_glob )

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
  END SUBROUTINE free_global_sound



!------------------------------------------------------------------------------
! PURPOSE:  Free objects created by to_global_sonde_sfc().  
!
! METHOD:   Simple bookeeping.  
!
!  HISTORY:
!           10/10/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: par_util
!------------------------------------------------------------------------------
  SUBROUTINE free_global_sonde_sfc( iv_glob, re_glob, jo_grad_y_glob )

    IMPLICIT NONE

    ! task-global objects
    TYPE (ob_type), INTENT(INOUT) :: iv_glob        ! Innovation vector
    TYPE (y_type),  INTENT(INOUT) :: re_glob        ! residual vector
    TYPE (y_type),  INTENT(INOUT) :: jo_grad_y_glob ! Grad_y(Jo)
    ! Local declarations
    INTEGER :: n
    DEALLOCATE( iv_glob%sonde_sfc )
    DEALLOCATE( re_glob%sonde_sfc )
    DEALLOCATE( jo_grad_y_glob%sonde_sfc )
    RETURN
  END SUBROUTINE free_global_sonde_sfc



!------------------------------------------------------------------------------
! PURPOSE:  Free objects created by to_global_synop().  
!
! METHOD:   Simple bookeeping.  
!
!  HISTORY:
!           10/10/2005 - Creation.                          Tom Henderson
!
! PARENT_MODULE: par_util
!------------------------------------------------------------------------------
  SUBROUTINE free_global_synop( iv_glob, re_glob, jo_grad_y_glob )

    IMPLICIT NONE

    ! task-global objects
    TYPE (ob_type), INTENT(INOUT) :: iv_glob        ! Innovation vector
    TYPE (y_type),  INTENT(INOUT) :: re_glob        ! residual vector
    TYPE (y_type),  INTENT(INOUT) :: jo_grad_y_glob ! Grad_y(Jo)
    ! Local declarations
    INTEGER :: n
    DEALLOCATE( iv_glob%synop )
    DEALLOCATE( re_glob%synop )
    DEALLOCATE( jo_grad_y_glob%synop )
    RETURN
  END SUBROUTINE free_global_synop

