SUBROUTINE DA_Calculate_Residual_Gpspw( oi, y, re,                        &
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

   IF ( oi%num_Gpspw > 0 ) THEN

      n_obs_bad % q % num = number(0, 0, 0)

      DO n=1, oi%num_Gpspw

      if( oi%gpspw(n)%loc%proc_domain_with_halo ) then

      np_available = np_available + 1

         re%gpspw(n)%tpw = residual(n, 0, y%gpspw(n)%tpw, &
     &                     oi%gpspw(n)%tpw, n_obs_bad % q )
      endif

      END DO

      np_missing  = np_missing  + n_obs_bad % q % num % miss 
      np_bad_data = np_bad_data + n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % q % num % use
   
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Gpspw

