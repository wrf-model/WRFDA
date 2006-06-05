
! This subroutine ensures that exactly the same code is used for both 
! serial and parallel computations when testing_dm_bitwise_exact is set.  
SUBROUTINE compute_jo_sound_uvtq( iv, re, jo_grad_y, jo )

  TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
  TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
  TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
  TYPE (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.
  ! Local declarations
  INTEGER :: n, k
  DO n=1, iv%num_sound
    IF ( iv%sound(n)%loc%proc_domain ) THEN
      DO k=1, iv%sound(n)%info%levels
        jo % sound_u = jo % sound_u - &
                       re%sound(n)%u(k) * jo_grad_y%sound(n)%u(k)
        jo % sound_v = jo % sound_v - &
                       re%sound(n)%v(k) * jo_grad_y%sound(n)%v(k)
        jo % sound_t = jo % sound_t - &
                       re%sound(n)%t(k) * jo_grad_y%sound(n)%t(k)
        jo % sound_q = jo % sound_q - &
                       re%sound(n)%q(k) * jo_grad_y%sound(n)%q(k)
      ENDDO
    ENDIF
  END DO

END SUBROUTINE compute_jo_sound_uvtq




