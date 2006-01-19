
! This subroutine ensures that exactly the same code is used for both
! serial and parallel computations when testing_dm_bitwise_exact is set.
SUBROUTINE compute_jo_sonde_sfc_uvtq( iv, re, jo_grad_y, jo )
   TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
   TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
   TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
   TYPE (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.
   ! Local declarations
   INTEGER :: n

   DO n=1, iv%num_sound

       if( iv%sonde_sfc(n)%loc%proc_domain ) then

         jo % sonde_sfc_u = jo % sonde_sfc_u - re%sonde_sfc(n)%u * jo_grad_y%sonde_sfc(n)%u
         jo % sonde_sfc_v = jo % sonde_sfc_v - re%sonde_sfc(n)%v * jo_grad_y%sonde_sfc(n)%v
         jo % sonde_sfc_t = jo % sonde_sfc_t - re%sonde_sfc(n)%t * jo_grad_y%sonde_sfc(n)%t
         jo % sonde_sfc_p = jo % sonde_sfc_p - re%sonde_sfc(n)%p * jo_grad_y%sonde_sfc(n)%p
         jo % sonde_sfc_q = jo % sonde_sfc_q - re%sonde_sfc(n)%q * jo_grad_y%sonde_sfc(n)%q

       endif

   END DO
END SUBROUTINE compute_jo_sonde_sfc_uvtq




