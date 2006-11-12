subroutine da_calculate_residual_bogus( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if ( oi%num_Bogus > 0 ) then
      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)
      n_obs_bad % slp % num = number(0, 0, 0)

      do n=1, oi%num_Bogus
         do k=1, oi%bogus(n)%info%levels
            np_available = np_available + 4
            re%Bogus(n)%u(k) = da_residual( n, k, y%Bogus(n)%u(k), &
                                     oi%Bogus(n)%u(k), n_obs_bad % u )
            re%Bogus(n)%v(k) = da_residual( n, k, y%Bogus(n)%v(k), &
                                     oi%Bogus(n)%v(k), n_obs_bad % v )
            re%Bogus(n)%t(k) = da_residual( n, k, y%Bogus(n)%t(k), &
                                     oi%Bogus(n)%t(k), n_obs_bad % t )
            re%Bogus(n)%q(k) = da_residual( n, k, y%Bogus(n)%q(k), &
                                     oi%Bogus(n)%q(k), n_obs_bad % q )
         end do

         np_available = np_available + 1

         re%Bogus(n)%slp = da_residual( n, 0, y%Bogus(n)%slp,  &
                                 oi%Bogus(n)%slp, n_obs_bad % slp ) 
      end do

      np_missing = np_missing + n_obs_bad % u % num % miss + &
                  n_obs_bad % v % num % miss + n_obs_bad % slp % num % miss + &
                  n_obs_bad % t % num % miss + n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                   n_obs_bad % v % num % bad + n_obs_bad % slp % num % bad  + &
                   n_obs_bad % t % num % bad + n_obs_bad % q % num % bad
     np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                   n_obs_bad % v % num % use + n_obs_bad % slp % num % use  + &
                   n_obs_bad % t % num % use + n_obs_bad % q % num % use

   end if

end subroutine da_calculate_residual_bogus
