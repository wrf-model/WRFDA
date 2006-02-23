SUBROUTINE DA_Calculate_Jo_and_GradY_Synop( iv, re, jo, jo_grad_y )

  IMPLICIT NONE

  TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
  TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
  TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
  type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

  INTEGER                      :: n
  LOGICAL, EXTERNAL            :: wrf_dm_on_monitor
  ! the following "global" objects are used only when testing
  TYPE (ob_type) :: iv_glob         ! Global Innovation vector (O-B).
  TYPE (y_type)  :: re_glob         ! Global Residual vector (O-A).
  TYPE (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

  jo % synop_u = 0.0
  jo % synop_v = 0.0
  jo % synop_t = 0.0
  jo % synop_p = 0.0
  jo % synop_q = 0.0

  IF ( testing_dm_bitwise_exact ) THEN
    IF ( iv%num_synop_glo == 0 ) RETURN
  ELSE
    IF ( iv%num_synop < 1 ) return
  ENDIF

  DO n=1, iv%num_synop

    if( iv%synop(n)%loc%proc_domain_with_halo ) then

      jo_grad_y%synop(n)%u = -re%synop(n)%u / &
                            ( iv%synop(n)%u%error * iv%synop(n)%u%error)
      jo_grad_y%synop(n)%v = -re%synop(n)%v / &
                            ( iv%synop(n)%v%error * iv%synop(n)%v%error)
      jo_grad_y%synop(n)%t = -re%synop(n)%t / &
                            ( iv%synop(n)%t%error * iv%synop(n)%t%error)
      jo_grad_y%synop(n)%p = -re%synop(n)%p / &
                            ( iv%synop(n)%p%error * iv%synop(n)%p%error)
      jo_grad_y%synop(n)%q = -re%synop(n)%q / &
                            ( iv%synop(n)%q%error * iv%synop(n)%q%error)
    endif
  END DO

  ! Bitwise-exact reduction preserves operation order of serial code for
  ! testing, at the cost of much-increased run-time.  Turn it off when not
  ! testing.  This will always be .FALSE. for a serial run.
  IF ( testing_dm_bitwise_exact ) THEN
    ! collect all obs in serial order and allocate global objects
    CALL to_global_synop( iv,      re,      jo_grad_y, &
                          iv_glob, re_glob, jo_grad_y_glob )
    ! perform remaining computations
    CALL compute_jo_synop_uvtq( iv_glob, re_glob, jo_grad_y_glob, jo )
    ! free global objects
    CALL free_global_synop( iv_glob, re_glob, jo_grad_y_glob )
  ELSE
    ! perform remaining computations
    CALL compute_jo_synop_uvtq( iv, re, jo_grad_y, jo )
  ENDIF

  jo % synop_u = 0.5 * jo % synop_u
  jo % synop_v = 0.5 * jo % synop_v
  jo % synop_t = 0.5 * jo % synop_t
  jo % synop_p = 0.5 * jo % synop_p
  jo % synop_q = 0.5 * jo % synop_q

  RETURN

END SUBROUTINE DA_Calculate_Jo_and_GradY_Synop


