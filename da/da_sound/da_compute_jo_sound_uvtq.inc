
! This subroutine ensures that exactly the same code is used for both 
! serial and parallel computations when testing_dm_bitwise_exact is set.  
subroutine da_compute_jo_sound_uvtq( iv, re, jo_grad_y, jo )

  type (ob_type), intent(in   ) :: iv         ! Innovation vector.
  type (y_type),  intent(in   ) :: re         ! Residual vector.
  type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
  type (jo_type), intent(inout) :: jo         ! Obs cost function.
  ! Local declarations
  integer :: n, k
  do n=1, iv%num_sound
    if ( iv%sound(n)%loc%proc_domain ) then
      do k=1, iv%sound(n)%info%levels
        jo % sound_u = jo % sound_u - &
                       re%sound(n)%u(k) * jo_grad_y%sound(n)%u(k)
        jo % sound_v = jo % sound_v - &
                       re%sound(n)%v(k) * jo_grad_y%sound(n)%v(k)
        jo % sound_t = jo % sound_t - &
                       re%sound(n)%t(k) * jo_grad_y%sound(n)%t(k)
        jo % sound_q = jo % sound_q - &
                       re%sound(n)%q(k) * jo_grad_y%sound(n)%q(k)
      end do
    end if
  end do

end subroutine da_compute_jo_sound_uvtq


