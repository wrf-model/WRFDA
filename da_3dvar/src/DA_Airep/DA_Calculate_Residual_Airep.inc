SUBROUTINE DA_Calculate_Residual_Airep( oi, y, re,                        &
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

   IF ( oi%num_Airep > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)

      DO n=1, oi%num_Airep

      if( oi%airep(n)%loc%proc_domain_with_halo ) then

      do k=1, oi%Airep(n)%info%levels

         np_available = np_available + 3

         re%Airep(n)%u(k) = residual( n, k, y%Airep(n)%u(k), &
                                   oi%Airep(n)%u(k), n_obs_bad % u )
         re%Airep(n)%v(k) = residual( n, k, y%Airep(n)%v(k), &
                                   oi%Airep(n)%v(k), n_obs_bad % v )
         re%Airep(n)%t(k) = residual( n, k, y%Airep(n)%t(k), &
                                   oi%Airep(n)%t(k), n_obs_bad % t )
      end do

      endif

      END DO

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss + n_obs_bad % t % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad + n_obs_bad % t % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use + n_obs_bad % t % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Airep
