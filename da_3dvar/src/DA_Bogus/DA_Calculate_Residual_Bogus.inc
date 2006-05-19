SUBROUTINE DA_Calculate_Residual_Bogus( oi, y, re,                        &
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

   IF ( oi%num_Bogus > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)
      n_obs_bad % slp % num = number(0, 0, 0)

      DO n=1, oi%num_Bogus

      if( oi%bogus(n)%loc%proc_domain_with_halo ) then
 
      do k=1, oi%bogus(n)%info%levels

         np_available = np_available + 4

         re%Bogus(n)%u(k) = residual( n, k, y%Bogus(n)%u(k), &
                                      oi%Bogus(n)%u(k), n_obs_bad % u )
         re%Bogus(n)%v(k) = residual( n, k, y%Bogus(n)%v(k), &
                                      oi%Bogus(n)%v(k), n_obs_bad % v )
         re%Bogus(n)%t(k) = residual( n, k, y%Bogus(n)%t(k), &
                                      oi%Bogus(n)%t(k), n_obs_bad % t )
         re%Bogus(n)%q(k) = residual( n, k, y%Bogus(n)%q(k), &
                                      oi%Bogus(n)%q(k), n_obs_bad % q )
      end do

      np_available = np_available + 1
 
      re%Bogus(n)%slp = residual( n, 0, y%Bogus(n)%slp,  &
                                  oi%Bogus(n)%slp, n_obs_bad % slp ) 
      end if
      END DO

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss + n_obs_bad % slp % num % miss + &
                   n_obs_bad % t % num % miss + n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad + n_obs_bad % slp % num % bad  + &
                    n_obs_bad % t % num % bad + n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use + n_obs_bad % slp % num % use  + &
                    n_obs_bad % t % num % use + n_obs_bad % q % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Bogus
