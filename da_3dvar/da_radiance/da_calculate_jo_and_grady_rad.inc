
SUBROUTINE da_calculate_jo_and_grady_rad( iv, re, jo, jo_grad_y ) 
!------------------------------------------------------------------------------
!  PURPOSE: Calculate Gradient_y i and cost function Jo for radiance data.
!
!  METHOD:  grad_y = -R^-1 ( d - H delta_x )
!               Jo = -( d - H delta_x ) grad_y
!
!  HISTORY: 08/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

  TYPE (ob_type), INTENT(IN)    :: iv          ! Innovation vector.
  TYPE (y_type) , INTENT(IN)    :: re          ! Residual vector.
  TYPE (y_type) , INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
  TYPE (jo_type), INTENT(INOUT) :: jo          ! Obs cost function.

  INTEGER                       :: n, k, i

  IF ( iv%num_inst < 1 ) RETURN

  ALLOCATE ( jo % rad(1:iv%num_inst) )

  DO i =1, iv%num_inst
    jo % rad(i) = 0.0
    IF ( iv%instid(i)%num_rad < 1 ) CYCLE

    DO n=1, iv%instid(i)%num_rad
      IF (iv%instid(i)%rad(n)%loc%proc_domain_with_halo) THEN
        DO k=1, iv%instid(i)%nchan
           jo_grad_y%instid(i)%rad(n)%tb(k) = -re%instid(i)%rad(n)%tb(k) / &
                                     ( iv%instid(i)%rad(n)%tb(k)%error * &
                                       iv%instid(i)%rad(n)%tb(k)%error )
        END DO
      END IF
      IF (iv%instid(i)%rad(n)%loc%proc_domain) THEN
        DO k=1, iv%instid(i)%nchan
          jo % rad(i) = jo % rad(i) - &
            re%instid(i)%rad(n)%tb(k) * jo_grad_y%instid(i)%rad(n)%tb(k)
        END DO
      END IF
    END DO
    jo % rad(i) = 0.5 * jo % rad(i)
  END DO

  RETURN

END SUBROUTINE da_calculate_jo_and_grady_rad

