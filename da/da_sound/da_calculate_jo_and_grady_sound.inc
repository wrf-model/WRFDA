subroutine da_calculate_jo_and_grady_sound( iv, re, jo, jo_grad_y )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN   ) :: iv         ! Innovation vector.
   TYPE (y_type),  INTENT(IN   ) :: re         ! Residual vector.
   TYPE (y_type),  INTENT(INOUT) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), INTENT(INOUT) :: jo         ! Obs cost function.

   INTEGER                      :: n, k
   ! the following "global" objects are used only when testing
   TYPE (ob_type) :: iv_glob         ! Global Innovation vector (O-B).
   TYPE (y_type)  :: re_glob         ! Global Residual vector (O-A).
   TYPE (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

   jo % sound_u = 0.0
   jo % sound_v = 0.0
   jo % sound_t = 0.0
   jo % sound_q = 0.0

   IF ( testing_dm_exact ) THEN
      IF ( iv%num_sound_glo == 0 ) RETURN
   ELSE
      IF ( iv%num_sound < 0 ) RETURN
   ENDIF

   DO n=1, iv%num_sound
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
   END DO

   ! Bitwise-exact reduction preserves operation order of serial code for 
   ! testing, at the cost of much-increased run-time.  Turn it off when not 
   ! testing.  This will always be .FALSE. for a serial or 1-MPI-process run.  
   IF ( testing_dm_exact ) THEN
      ! collect all obs in serial order and allocate global objects
      call da_to_global_sound( iv,      re,      jo_grad_y, &
                           iv_glob, re_glob, jo_grad_y_glob )
      ! perform remaining computations
      call da_compute_jo_sound_uvtq( iv_glob, re_glob, jo_grad_y_glob, jo )
      ! free global objects
      call da_free_global_sound( iv_glob, re_glob, jo_grad_y_glob )
   ELSE
      ! perform remaining computations
      call da_compute_jo_sound_uvtq( iv, re, jo_grad_y, jo )
   ENDIF

   jo % sound_u = 0.5 * jo % sound_u
   jo % sound_v = 0.5 * jo % sound_v
   jo % sound_t = 0.5 * jo % sound_t
   jo % sound_q = 0.5 * jo % sound_q

end subroutine DA_Calculate_Jo_and_GradY_sound


