
! This subroutine ensures that exactly the same code is used for both
! serial and parallel computations when testing_dm_bitwise_exact is set.

subroutine da_compute_jo_sonde_sfc_uvtq( iv, re, jo_grad_y, jo )

  type (ob_type), intent(in   ) :: iv         ! Innovation vector.
  type (y_type),  intent(in   ) :: re         ! Residual vector.
  type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
  type (jo_type), intent(inout) :: jo         ! Obs cost function.
  ! Local declarations
  integer :: n

  do n=1, iv%num_sound
    if( iv%sonde_sfc(n)%loc%proc_domain ) then
      jo % sonde_sfc_u = jo % sonde_sfc_u - re%sonde_sfc(n)%u * jo_grad_y%sonde_sfc(n)%u
      jo % sonde_sfc_v = jo % sonde_sfc_v - re%sonde_sfc(n)%v * jo_grad_y%sonde_sfc(n)%v
      jo % sonde_sfc_t = jo % sonde_sfc_t - re%sonde_sfc(n)%t * jo_grad_y%sonde_sfc(n)%t
      jo % sonde_sfc_p = jo % sonde_sfc_p - re%sonde_sfc(n)%p * jo_grad_y%sonde_sfc(n)%p
      jo % sonde_sfc_q = jo % sonde_sfc_q - re%sonde_sfc(n)%q * jo_grad_y%sonde_sfc(n)%q
    end if
  end do

end subroutine da_compute_jo_sonde_sfc_uvtq


