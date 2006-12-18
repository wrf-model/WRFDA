SUBROUTINE DA_Calculate_Residual_Ships( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n

   IF ( oi%num_ships > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % p % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)

      DO n=1, oi%num_ships

       if( oi%ships(n)%loc%proc_domain_with_halo ) then

         np_available = np_available + 5

         re%ships(n)%u = residual( n, 0, y%ships(n)%u, &
                                      oi%ships(n)%u, n_obs_bad % u )
         re%ships(n)%v = residual( n, 0, y%ships(n)%v, &
                                   oi%ships(n)%v, n_obs_bad % v )
         re%ships(n)%t = residual( n, 0, y%ships(n)%t, &
                                   oi%ships(n)%t, n_obs_bad % t )
         re%ships(n)%p = residual( n, 0, y%ships(n)%p, &
                                   oi%ships(n)%p, n_obs_bad % p )
         re%ships(n)%q = residual( n, 0, y%ships(n)%q, &
                                   oi%ships(n)%q, n_obs_bad % q )
       endif

      END DO

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
                   n_obs_bad % p % num % miss + n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
                    n_obs_bad % p % num % bad + n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
                    n_obs_bad % p % num % use + n_obs_bad % q % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Ships
