SUBROUTINE DA_Calculate_Residual_Radar( oi, y, re,                        &
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

   IF ( oi%num_Radar > 0 ) THEN

      n_obs_bad % rv % num = number(0, 0, 0)
      n_obs_bad % rf % num = number(0, 0, 0)

      DO n=1, oi%num_Radar

      if( oi%Radar(n)%loc%proc_domain_with_halo ) then

      do k=1, oi%Radar(n)%info%levels

        IF ( USE_Radar_rv ) THEN

          np_available = np_available + 1

          re%Radar(n)%rv(k) = residual( n, k, y%Radar(n)%rv(k), &
                                        oi%Radar(n)%rv(k), n_obs_bad % rv )
        ENDIF

        IF ( USE_Radar_rf ) THEN

          np_available = np_available + 1

          re%Radar(n)%rf(k) = residual( n, k, y%Radar(n)%rf(k), &
                                        oi%Radar(n)%rf(k), n_obs_bad % rf )
        ENDIF

      end do

      end if

      END DO

      np_missing  = np_missing  + n_obs_bad % rv % num % miss  &
                                + n_obs_bad % rf % num % miss
      np_bad_data = np_bad_data + n_obs_bad % rv % num % bad   &
                                + n_obs_bad % rf % num % bad
      np_obs_used = np_obs_used + n_obs_bad % rv % num % use   &
                                + n_obs_bad % rf % num % use
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Radar
