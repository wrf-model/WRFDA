SUBROUTINE DA_Calculate_Jo_and_GradY_sound( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)   :: iv          ! Innovation vector.
   TYPE (y_type), INTENT(IN)    :: re          ! Residual vector.
   TYPE (y_type), INTENT(INOUT) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT):: jo          ! Obs cost function.

   INTEGER                      :: n, k

   jo % sound_u = 0.0
   jo % sound_v = 0.0
   jo % sound_t = 0.0
   jo % sound_q = 0.0

   IF ( iv%num_sound < 0 ) RETURN

   DO n=1, iv%num_sound
       if( iv%sound(n)%loc%proc_domain_with_halo ) then

         do k=1, iv%sound(n)%info%levels

            jo_grad_y%sound(n)%u(k) = -re%sound(n)%u(k) / &
                                      ( iv%sound(n)%u(k)%error * &
                                        iv%sound(n)%u(k)%error )
            jo_grad_y%sound(n)%v(k) = -re%sound(n)%v(k) / &
                                      ( iv%sound(n)%v(k)%error * &
                                        iv%sound(n)%v(k)%error )
            jo_grad_y%sound(n)%t(k) = -re%sound(n)%t(k) / &
                                      ( iv%sound(n)%t(k)%error * &
                                        iv%sound(n)%t(k)%error )
            jo_grad_y%sound(n)%q(k) = -re%sound(n)%q(k) / &
                                      ( iv%sound(n)%q(k)%error * &
                                        iv%sound(n)%q(k)%error )
          end do

        endif

        if( iv%sound(n)%loc%proc_domain ) then

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

        endif

   END DO

   jo % sound_u = 0.5 * jo % sound_u
   jo % sound_v = 0.5 * jo % sound_v
   jo % sound_t = 0.5 * jo % sound_t
   jo % sound_q = 0.5 * jo % sound_q

END SUBROUTINE DA_Calculate_Jo_and_GradY_sound

