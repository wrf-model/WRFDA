subroutine da_compute_jo_airsr_tq( iv, re, jo_grad_y, jo )

  TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
  TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
  TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
  TYPE (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.
  ! Local declarations
  INTEGER :: n, k
  DO n=1, iv%num_airsr
    IF ( iv%airsr(n)%loc%proc_domain ) THEN
      DO k=1, iv%airsr(n)%info%levels
        jo % airsr_t = jo % airsr_t - &
                       re%airsr(n)%t(k) * jo_grad_y%airsr(n)%t(k)
        jo % airsr_q = jo % airsr_q - &
                       re%airsr(n)%q(k) * jo_grad_y%airsr(n)%q(k)
      ENDDO
    ENDIF
  END DO

end subroutine da_compute_jo_airsr_tq
