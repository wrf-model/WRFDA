SUBROUTINE DA_Calculate_Residual_Satem( oi, y, re,                        &
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

   IF ( oi%num_Satem > 0 ) THEN

      n_obs_bad % thickness % num = number(0, 0, 0)

      DO n=1, oi%num_Satem

      if( oi%satem(n)%loc%proc_domain_with_halo ) then

      do k=1, oi%satem(n)%info%levels
         np_available = np_available + 1
         re%Satem(n)%thickness(k) = residual( n, k, y%Satem(n)%thickness(k), &
                              oi%Satem(n)%thickness(k), n_obs_bad % thickness )
      end do

      endif

      END DO

      np_missing = np_missing + n_obs_bad % thickness % num % miss
      np_bad_data = np_bad_data + n_obs_bad % thickness % num % bad
      np_obs_used = np_obs_used + n_obs_bad % thickness % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Satem

