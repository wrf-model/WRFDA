subroutine da_cal_residual_ssmt1( iv, y, re,                        &
                                  np_missing, np_bad_data,          &
                                  np_obs_used, np_available )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: iv     ! Innovation vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n, k

   if ( iv%num_ssmt1 > 0 ) then

      n_obs_bad % t % num = number(0, 0, 0)

      DO n=1, iv%num_ssmt1
         do k=1, iv%ssmt1(n)%info%levels
            np_available = np_available + 1

            re%ssmt1(n)%t(k) = residual( n, k, y%ssmt1(n)%t(k), &
                                         iv%ssmt1(n)%t(k), n_obs_bad % t )
         end do
      end do

      np_missing = np_missing + n_obs_bad % t % num % miss
      np_bad_data = np_bad_data + n_obs_bad % t % num % bad
      np_obs_used = np_obs_used + n_obs_bad % t % num % use
                    
   end if

END subroutine da_cal_residual_ssmt1
