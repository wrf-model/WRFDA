subroutine da_calculate_residual_airsr( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   !----------------------------------------------------------------------
   ! Purpose: Calculates residual at AIRS retrieval locations                   
   !----------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(INOUT)     :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n, k

   IF ( oi%num_airsr > 0 ) THEN
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)

      DO n=1, oi%num_airsr
         do k=1, oi%airsr(n)%info%levels
           np_available = np_available + 4

           re%airsr(n)%t(k) = da_residual( n, k, y%airsr(n)%t(k), &
                                      oi%airsr(n)%t(k), n_obs_bad % t )
           re%airsr(n)%q(k) = da_residual( n, k, y%airsr(n)%q(k), &
                                      oi%airsr(n)%q(k), n_obs_bad % q )
         end do
      END DO

      np_missing = np_missing + n_obs_bad % t % num % miss + &
                               n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % t % num % bad + &
                                 n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % t % num % use + &
                                 n_obs_bad % q % num % use

   ENDIF

end subroutine da_calculate_residual_airsr

