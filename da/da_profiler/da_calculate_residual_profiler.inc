SUBROUTINE DA_Calculate_Residual_Profiler( oi, y, re,                        &
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

   IF ( oi%num_Profiler > 0 ) THEN

      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)

      DO n=1, oi%num_Profiler

      if( oi%profiler(n)%loc%proc_domain_with_halo ) then

      do k=1, oi%Profiler(n)%info%levels

         np_available = np_available + 2

         re%Profiler(n)%u(k) = residual( n, k, y%Profiler(n)%u(k), &
                                   oi%Profiler(n)%u(k), n_obs_bad % u )
         re%Profiler(n)%v(k) = residual( n, k, y%Profiler(n)%v(k), &
                                   oi%Profiler(n)%v(k), n_obs_bad % v )
      end do

      endif

      END DO

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                   n_obs_bad % v % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                    n_obs_bad % v % num % bad 
      np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                    n_obs_bad % v % num % use
   ENDIF

END SUBROUTINE DA_Calculate_Residual_Profiler

