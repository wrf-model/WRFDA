SUBROUTINE DA_Calculate_Residual_Sound( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n, k

   IF ( oi%num_Sound > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)

      DO n=1, oi%num_Sound

      if( oi%sound(n)%loc%proc_domain_with_halo ) then

      do k=1, oi%sound(n)%info%levels

         np_available = np_available + 4

         re%Sound(n)%u(k) = residual( n, k, y%Sound(n)%u(k), &
                                      oi%Sound(n)%u(k), n_obs_bad % u )
         re%Sound(n)%v(k) = residual( n, k, y%Sound(n)%v(k), &
                                      oi%Sound(n)%v(k), n_obs_bad % v )
         re%Sound(n)%t(k) = residual( n, k, y%Sound(n)%t(k), &
                                      oi%Sound(n)%t(k), n_obs_bad % t )
         re%Sound(n)%q(k) = residual( n, k, y%Sound(n)%q(k), &
                                      oi%Sound(n)%q(k), n_obs_bad % q )
      end do

      endif

      END DO

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
                   n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
                    n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
                    n_obs_bad % q % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Sound

