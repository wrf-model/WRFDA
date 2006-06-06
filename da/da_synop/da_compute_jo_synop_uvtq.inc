
! This subroutine ensures that exactly the same code is used for both
! serial and parallel computations when testing_dm_bitwise_exact is set.
SUBROUTINE da_compute_jo_synop_uvtq( iv, re, jo_grad_y, jo )
  TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
  TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
  TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
  TYPE (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.
  ! Local declarations
  INTEGER :: n

  DO n=1, iv%num_synop
    if( iv%synop(n)%loc%proc_domain ) then
      jo % synop_u = jo % synop_u - re%synop(n)%u * jo_grad_y%synop(n)%u
      jo % synop_v = jo % synop_v - re%synop(n)%v * jo_grad_y%synop(n)%v
      jo % synop_t = jo % synop_t - re%synop(n)%t * jo_grad_y%synop(n)%t
      jo % synop_p = jo % synop_p - re%synop(n)%p * jo_grad_y%synop(n)%p
      jo % synop_q = jo % synop_q - re%synop(n)%q * jo_grad_y%synop(n)%q
    endif
  END DO

END SUBROUTINE da_compute_jo_synop_uvtq




