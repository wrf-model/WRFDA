SUBROUTINE DA_Calculate_Residual_Satob( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual vector (O-A).

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n

   IF ( oi%num_satob > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)

      DO n=1, oi%num_satob

       if( oi%satob(n)%loc%proc_domain_with_halo ) then

         np_available = np_available + 2

         re%satob(n)%u = residual( n, 0, y%satob(n)%u, &
                                      oi%satob(n)%u, n_obs_bad % u )
         re%satob(n)%v = residual( n, 0, y%satob(n)%v, &
                                   oi%satob(n)%v, n_obs_bad % v )
                          
       endif
         
      END DO
      
      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use

   ENDIF

END SUBROUTINE DA_Calculate_Residual_Satob

