subroutine da_cal_residual_ssmt2( iv, y, re,                        &
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

   if ( iv%num_ssmt2 > 0 ) then

      n_obs_bad % rh % num = number(0, 0, 0)

      DO n=1, iv%num_ssmt2
         do k=1, iv%ssmt2(n)%info%levels
            np_available = np_available + 1

            re%ssmt2(n)%rh(k) = residual( n, k, y%ssmt2(n)%rh(k), &
                                          iv%ssmt2(n)%rh(k), n_obs_bad % rh )
         end do
      end do

      np_missing = np_missing + n_obs_bad % rh % num % miss
      np_bad_data = np_bad_data + n_obs_bad % rh % num % bad
      np_obs_used = np_obs_used + n_obs_bad % rh % num % use
                    
   end if

END subroutine da_cal_residual_ssmt2
