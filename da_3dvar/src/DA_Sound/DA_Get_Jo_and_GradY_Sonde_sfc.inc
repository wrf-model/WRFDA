SUBROUTINE DA_Get_Jo_and_GradY_Sonde_sfc( iv, re, jo, jo_grad_y )

  IMPLICIT NONE

  TYPE (ob_type), INTENT(IN)    :: iv          ! Innovation vector.
  TYPE (y_type),  INTENT(IN)    :: re          ! Residual vector.
  TYPE (y_type),  INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
  type (jo_type), INTENT(INOUT) :: jo          ! Obs cost function.

  INTEGER                      :: n
  LOGICAL, EXTERNAL            :: wrf_dm_on_monitor
  ! the following "global" objects are used only when testing
  TYPE (ob_type) :: iv_glob         ! Global Innovation vector (O-B).
  TYPE (y_type)  :: re_glob         ! Global Residual vector (O-A).
  TYPE (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

  jo % sonde_sfc_u = 0.0
  jo % sonde_sfc_v = 0.0
  jo % sonde_sfc_t = 0.0
  jo % sonde_sfc_p = 0.0
  jo % sonde_sfc_q = 0.0

  IF ( testing_dm_bitwise_exact ) THEN
    IF ( iv%num_sound_glo == 0 ) RETURN
  ELSE
    IF ( iv%num_sound < 1 ) return
  ENDIF

  DO n=1, iv%num_sound
    if( iv%sonde_sfc(n)%loc%proc_domain_with_halo ) then

      jo_grad_y%sonde_sfc(n)%u = -re%sonde_sfc(n)%u / &
                               ( iv%sonde_sfc(n)%u%error * iv%sonde_sfc(n)%u%error)
      jo_grad_y%sonde_sfc(n)%v = -re%sonde_sfc(n)%v / &
                               ( iv%sonde_sfc(n)%v%error * iv%sonde_sfc(n)%v%error)
      jo_grad_y%sonde_sfc(n)%t = -re%sonde_sfc(n)%t / &
                               ( iv%sonde_sfc(n)%t%error * iv%sonde_sfc(n)%t%error)
      jo_grad_y%sonde_sfc(n)%p = -re%sonde_sfc(n)%p / &
                               ( iv%sonde_sfc(n)%p%error * iv%sonde_sfc(n)%p%error)
      jo_grad_y%sonde_sfc(n)%q = -re%sonde_sfc(n)%q / &
                               ( iv%sonde_sfc(n)%q%error * iv%sonde_sfc(n)%q%error)
    endif
  END DO

  ! Bitwise-exact reduction preserves operation order of serial code for
  ! testing, at the cost of much-increased run-time.  Turn it off when not
  ! testing.  This will always be .FALSE. for a serial run.
  IF ( testing_dm_bitwise_exact ) THEN
    ! collect all obs in serial order and allocate global objects
    CALL to_global_sonde_sfc( iv,      re,      jo_grad_y, &
                          iv_glob, re_glob, jo_grad_y_glob )
    ! perform remaining computations
    CALL compute_jo_sonde_sfc_uvtq( iv_glob, re_glob, jo_grad_y_glob, jo )
    ! free global objects
    CALL free_global_sonde_sfc( iv_glob, re_glob, jo_grad_y_glob )
  ELSE
    ! perform remaining computations
    CALL compute_jo_sonde_sfc_uvtq( iv, re, jo_grad_y, jo )
  ENDIF

  jo % sonde_sfc_u = 0.5 * jo % sonde_sfc_u
  jo % sonde_sfc_v = 0.5 * jo % sonde_sfc_v
  jo % sonde_sfc_t = 0.5 * jo % sonde_sfc_t
  jo % sonde_sfc_p = 0.5 * jo % sonde_sfc_p
  jo % sonde_sfc_q = 0.5 * jo % sonde_sfc_q

  RETURN

END SUBROUTINE DA_Get_Jo_and_GradY_Sonde_sfc
